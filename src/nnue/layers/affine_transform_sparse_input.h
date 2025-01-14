/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2025 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

// Definition of layer AffineTransformSparseInput of NNUE evaluation function

#ifndef NNUE_LAYERS_AFFINE_TRANSFORM_SPARSE_INPUT_H_INCLUDED
#define NNUE_LAYERS_AFFINE_TRANSFORM_SPARSE_INPUT_H_INCLUDED

#include <algorithm>
#include <array>
#include <cstdint>
#include <iostream>

#include "../../bitboard.h"
#include "../nnue_common.h"
#include "affine_transform.h"
#include "simd.h"

/*
  This file contains the definition for a fully connected layer (aka affine transform) with block sparse input.
*/

namespace Stockfish::Eval::NNUE::Layers {

#if (USE_AVX2)
alignas(CacheLineSize) static inline const
  std::array<std::array<std::uint8_t, 16>, 65536> lookup_indices = []() {
      std::array<std::array<std::uint8_t, 16>, 65536> v{};
      for (unsigned i = 1; i < 65536; ++i)
      {
          std::uint64_t j = i, k = 0;
          while (j)
              v[i][k++] = pop_lsb(j);
      }
      return v;
  }();
#elif (USE_SSSE3 | (USE_NEON >= 8))
alignas(CacheLineSize) static inline const
  std::array<std::array<std::uint16_t, 8>, 256> lookup_indices = []() {
      std::array<std::array<std::uint16_t, 8>, 256> v{};
      for (unsigned i = 1; i < 256; ++i)
      {
          std::uint64_t j = i, k = 0;
          while (j)
              v[i][k++] = pop_lsb(j);
      }
      return v;
  }();
#endif

#if (USE_SSSE3 | (USE_NEON >= 8))
// Find indices of nonzero numbers in an int32_t array
template<const IndexType InputDimensions>
void find_nnz(const std::int32_t* input, std::uint16_t* out, IndexType& count_out) {
    #if defined(USE_SSSE3)
        #if defined(USE_AVX512)
    using vecIn_t = __m512i;
            #define vec_nnz(a) _mm512_cmpgt_epi32_mask(a, _mm512_setzero_si512())
        #elif defined(USE_AVX2)
    using vecIn_t = __m256i;
            #if defined(USE_VNNI) && !defined(USE_AVXVNNI)
                #define vec_nnz(a) _mm256_cmpgt_epi32_mask(a, _mm256_setzero_si256())
            #else
                #define vec_nnz(a) \
                    _mm256_movemask_ps( \
                      _mm256_castsi256_ps(_mm256_cmpgt_epi32(a, _mm256_setzero_si256())))
            #endif
        #elif defined(USE_SSSE3)
    using vecIn_t = __m128i;
            #define vec_nnz(a) \
                _mm_movemask_ps(_mm_castsi128_ps(_mm_cmpgt_epi32(a, _mm_setzero_si128())))
        #endif
    #endif

    #if defined(USE_AVX2)
    using vecOut_t = __m256i;
        #define vecOut_zero _mm256_setzero_si256()
        #define vecOut_set_16(a) _mm256_set1_epi16(a)
        #define vecOut_load(a) _mm256_cvtepi8_epi16(_mm_load_si128(a))
        #define vecOut_storeu(a, b) _mm256_storeu_si256(a, b)
        #define vecOut_add(a, b) _mm256_add_epi16(a, b)
    #elif defined(USE_SSSE3)
    using vecOut_t = __m128i;
        #define vecOut_zero _mm_setzero_si128()
        #define vecOut_set_16(a) _mm_set1_epi16(a)
        #define vecOut_load(a) _mm_load_si128(a)
        #define vecOut_storeu(a, b) _mm_storeu_si128(a, b)
        #define vecOut_add(a, b) _mm_add_epi16(a, b)
    #elif defined(USE_NEON)
    using vecIn_t                     = uint32x4_t;
    constexpr std::uint32_t Mask[4] = {1, 2, 4, 8};
        #define vec_nnz(a) vaddvq_u32(vandq_u32(vtstq_u32(a, a), vld1q_u32(Mask)))
    using vec128_t                  = uint16x8_t;
        #define vecOut_zero vdupq_n_u16(0)
        #define vecOut_set_16(a) vdupq_n_u16(a)
        #define vecOut_load(a) vld1q_u16(reinterpret_cast<const std::uint16_t*>(a))
        #define vecOut_storeu(a, b) vst1q_u16(reinterpret_cast<std::uint16_t*>(a), b)
        #define vecOut_add(a, b) vaddq_u16(a, b)
    #endif
#endif

#if (USE_AVX2)

    constexpr IndexType InputSimdWidth = sizeof(vecIn_t) / sizeof(std::int32_t);
    // Inputs are processed InputSimdWidth at a time and outputs are processed 16 at a time so we process in chunks of max(InputSimdWidth, 16)
    constexpr IndexType ChunkSize       = std::max<IndexType>(InputSimdWidth, 16);
    constexpr IndexType NumChunks       = InputDimensions / ChunkSize;
    constexpr IndexType InputsPerChunk  = ChunkSize / InputSimdWidth;
    constexpr IndexType OutputsPerChunk = ChunkSize / 16;

    const auto     inputVector = reinterpret_cast<const vecIn_t*>(input);
    IndexType      count       = 0;
    vecOut_t       base        = vecOut_zero;
    const vecOut_t increment   = vecOut_set_16(16);
    for (IndexType i = 0; i < NumChunks; ++i)
    {
        // bitmask of nonzero values in this chunk
        unsigned nnz = 0;
        for (IndexType j = 0; j < InputsPerChunk; ++j)
        {
            const vecIn_t inputChunk = inputVector[i * InputsPerChunk + j];
            nnz |= unsigned(vec_nnz(inputChunk)) << (j * InputSimdWidth);
        }
        for (IndexType j = 0; j < OutputsPerChunk; ++j)
        {
            const unsigned lookup  = (nnz >> (j * 16)) & 0xFFFF;
            const vecOut_t offsets = vecOut_load(reinterpret_cast<const __m128i*>(&lookup_indices[lookup]));
            vecOut_storeu(reinterpret_cast<vecOut_t*>(out + count), vecOut_add(base, offsets));
            count += popcount(lookup);
            base = vecOut_add(base, increment);
        }
    }
    count_out = count;    

#elif (USE_SSSE3 | (USE_NEON >= 8))

    constexpr IndexType InputSimdWidth = sizeof(vecIn_t) / sizeof(std::int32_t);
    // Inputs are processed InputSimdWidth at a time and outputs are processed 8 at a time so we process in chunks of max(InputSimdWidth, 8)
    constexpr IndexType ChunkSize       = std::max<IndexType>(InputSimdWidth, 8);
    constexpr IndexType NumChunks       = InputDimensions / ChunkSize;
    constexpr IndexType InputsPerChunk  = ChunkSize / InputSimdWidth;
    constexpr IndexType OutputsPerChunk = ChunkSize / 8;

    const auto     inputVector = reinterpret_cast<const vecIn_t*>(input);
    IndexType      count       = 0;
    vecOut_t       base        = vecOut_zero;
    const vecOut_t increment   = vecOut_set_16(8);
    for (IndexType i = 0; i < NumChunks; ++i)
    {
        // bitmask of nonzero values in this chunk
        unsigned nnz = 0;
        for (IndexType j = 0; j < InputsPerChunk; ++j)
        {
            const vecIn_t inputChunk = inputVector[i * InputsPerChunk + j];
            nnz |= unsigned(vec_nnz(inputChunk)) << (j * InputSimdWidth);
        }
        for (IndexType j = 0; j < OutputsPerChunk; ++j)
        {
            const unsigned lookup = (nnz >> (j * 8)) & 0xFF;
            const vecOut_t offsets = vecOut_load(reinterpret_cast<const vecOut_t*>(&lookup_indices[lookup]));
            vecOut_storeu(reinterpret_cast<vecOut_t*>(out + count), vecOut_add(base, offsets));
            count += popcount(lookup);
            base = vecOut_add(base, increment);
        }
    }
    count_out = count;
#endif

#undef vec_nnz
#undef vecOut_zero
#undef vecOut_set_16
#undef vecOut_load
#undef vecOut_storeu
#undef vecOut_add
}

// Sparse input implementation
template<IndexType InDims, IndexType OutDims>
class AffineTransformSparseInput {
   public:
    // Input/output type
    using InputType  = std::uint8_t;
    using OutputType = std::int32_t;

    // Number of input/output dimensions
    static constexpr IndexType InputDimensions  = InDims;
    static constexpr IndexType OutputDimensions = OutDims;

    static_assert(OutputDimensions % 16 == 0,
                  "Only implemented for OutputDimensions divisible by 16.");

    static constexpr IndexType PaddedInputDimensions =
      ceil_to_multiple<IndexType>(InputDimensions, MaxSimdWidth);
    static constexpr IndexType PaddedOutputDimensions =
      ceil_to_multiple<IndexType>(OutputDimensions, MaxSimdWidth);

#if (USE_SSSE3 | (USE_NEON >= 8))
    static constexpr IndexType ChunkSize = 4;
#else
    static constexpr IndexType ChunkSize = 1;
#endif

    using OutputBuffer = OutputType[PaddedOutputDimensions];

    // Hash value embedded in the evaluation file
    static constexpr std::uint32_t get_hash_value(std::uint32_t prevHash) {
        std::uint32_t hashValue = 0xCC03DAE4u;
        hashValue += OutputDimensions;
        hashValue ^= prevHash >> 1;
        hashValue ^= prevHash << 31;
        return hashValue;
    }

    static constexpr IndexType get_weight_index_scrambled(IndexType i) {
        return (i / ChunkSize) % (PaddedInputDimensions / ChunkSize) * OutputDimensions * ChunkSize
             + i / PaddedInputDimensions * ChunkSize + i % ChunkSize;
    }

    static constexpr IndexType get_weight_index(IndexType i) {
#if (USE_SSSE3 | (USE_NEON >= 8))
        return get_weight_index_scrambled(i);
#else
        return i;
#endif
    }

    // Read network parameters
    bool read_parameters(std::istream& stream) {
        read_little_endian<BiasType>(stream, biases, OutputDimensions);
        for (IndexType i = 0; i < OutputDimensions * PaddedInputDimensions; ++i)
            weights[get_weight_index(i)] = read_little_endian<WeightType>(stream);

        return !stream.fail();
    }

    // Write network parameters
    bool write_parameters(std::ostream& stream) const {
        write_little_endian<BiasType>(stream, biases, OutputDimensions);

        for (IndexType i = 0; i < OutputDimensions * PaddedInputDimensions; ++i)
            write_little_endian<WeightType>(stream, weights[get_weight_index(i)]);

        return !stream.fail();
    }
    // Forward propagation
    void propagate(const InputType* input, OutputType* output) const {

#if (USE_SSSE3 | (USE_NEON >= 8))
    #if defined(USE_AVX512)
        using invec_t  = __m512i;
        using outvec_t = __m512i;
        #define vec_set_32 _mm512_set1_epi32
        #define vec_add_dpbusd_32 Simd::m512_add_dpbusd_epi32
    #elif defined(USE_AVX2)
        using invec_t  = __m256i;
        using outvec_t = __m256i;
        #define vec_set_32 _mm256_set1_epi32
        #define vec_add_dpbusd_32 Simd::m256_add_dpbusd_epi32
    #elif defined(USE_SSSE3)
        using invec_t  = __m128i;
        using outvec_t = __m128i;
        #define vec_set_32 _mm_set1_epi32
        #define vec_add_dpbusd_32 Simd::m128_add_dpbusd_epi32
    #elif defined(USE_NEON_DOTPROD)
        using invec_t  = int8x16_t;
        using outvec_t = int32x4_t;
        #define vec_set_32(a) vreinterpretq_s8_u32(vdupq_n_u32(a))
        #define vec_add_dpbusd_32 Simd::dotprod_m128_add_dpbusd_epi32
    #elif defined(USE_NEON)
        using invec_t  = int8x16_t;
        using outvec_t = int32x4_t;
        #define vec_set_32(a) vreinterpretq_s8_u32(vdupq_n_u32(a))
        #define vec_add_dpbusd_32 Simd::neon_m128_add_dpbusd_epi32
    #endif
        static constexpr IndexType OutputSimdWidth = sizeof(outvec_t) / sizeof(OutputType);

        constexpr IndexType NumChunks = ceil_to_multiple<IndexType>(InputDimensions, 8) / ChunkSize;
        constexpr IndexType NumRegs   = OutputDimensions / OutputSimdWidth;
        std::uint16_t       nnz[NumChunks];
        IndexType           count;

        const auto input32 = reinterpret_cast<const std::int32_t*>(input);

        // Find indices of nonzero 32-bit blocks
        find_nnz<NumChunks>(input32, nnz, count);

        const outvec_t* biasvec = reinterpret_cast<const outvec_t*>(biases);
        outvec_t        acc[NumRegs];
        for (IndexType k = 0; k < NumRegs; ++k)
            acc[k] = biasvec[k];

        for (IndexType j = 0; j < count; ++j)
        {
            const auto    i  = nnz[j];
            const invec_t in = vec_set_32(input32[i]);
            const auto    col =
              reinterpret_cast<const invec_t*>(&weights[i * OutputDimensions * ChunkSize]);
            for (IndexType k = 0; k < NumRegs; ++k)
                vec_add_dpbusd_32(acc[k], in, col[k]);
        }

        outvec_t* outptr = reinterpret_cast<outvec_t*>(output);
        for (IndexType k = 0; k < NumRegs; ++k)
            outptr[k] = acc[k];
    #undef vec_set_32
    #undef vec_add_dpbusd_32
#else
        // Use dense implementation for the other architectures.
        affine_transform_non_ssse3<InputDimensions, PaddedInputDimensions, OutputDimensions>(
          output, weights, biases, input);
#endif
    }

   private:
    using BiasType   = OutputType;
    using WeightType = std::int8_t;

    alignas(CacheLineSize) BiasType biases[OutputDimensions];
    alignas(CacheLineSize) WeightType weights[OutputDimensions * PaddedInputDimensions];
};

}  // namespace Stockfish::Eval::NNUE::Layers

#endif  // #ifndef NNUE_LAYERS_AFFINE_TRANSFORM_SPARSE_INPUT_H_INCLUDED
