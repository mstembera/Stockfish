/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2020 The Stockfish developers (see AUTHORS file)

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

// Code for calculating NNUE evaluation function

#include <iostream>
#include <set>

#include "../evaluate.h"
#include "../position.h"
#include "../misc.h"
#include "../uci.h"
#include "../types.h"

#include "evaluate_nnue.h"

int TuneW[32] = {
  0, 1, 18, 8, -4, -11, -14, -3, 6, 2, 4, 6, 3, -9, 14, -1, -3, 1, -13, -10, 9, 4, -41, -2, -41, 12, 17, 20, 6, -6, 4, 6
};

int TuneB[32] = {
  -1636, -1296, 1434, -1881, -1544, 669, -454, 3617, -458, 11188, 2007, 3096, -3710, 6217, 1033, 4988,
  4811, -546, 8955, -2867, -2619, 6637, 14132, 2474, -7371, 2994, -3596, 5869, 53, -4729, -8574, -8788
};

#if 1
auto rangeFuncW = [](int m) { return std::pair<int, int>(std::max(m - 50, -127), std::min(m + 50, 127)); };
TUNE(SetRange(rangeFuncW), TuneW);

auto rangeFuncB = [](int m) { return std::pair<int, int>(m - 1500, m + 1500); };
TUNE(SetRange(rangeFuncB), TuneB);

UPDATE_ON_LAST();
#endif

namespace Eval::NNUE {

  // Input feature converter
  LargePagePtr<FeatureTransformer> feature_transformer;

  // Evaluation function
  AlignedPtr<Network> network;

  // Evaluation function file name
  std::string fileName;

  namespace Detail {

  // Initialize the evaluation function parameters
  template <typename T>
  void Initialize(AlignedPtr<T>& pointer) {

    pointer.reset(reinterpret_cast<T*>(std_aligned_alloc(alignof(T), sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  template <typename T>
  void Initialize(LargePagePtr<T>& pointer) {

    static_assert(alignof(T) <= 4096, "aligned_large_pages_alloc() may fail for such a big alignment requirement of T");
    pointer.reset(reinterpret_cast<T*>(aligned_large_pages_alloc(sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  // Read evaluation function parameters
  template <typename T>
  bool ReadParameters(std::istream& stream, T& reference) {

    std::uint32_t header;
    header = read_little_endian<std::uint32_t>(stream);
    if (!stream || header != T::GetHashValue()) return false;
    return reference.ReadParameters(stream);
  }

  }  // namespace Detail

  // Initialize the evaluation function parameters
  void Initialize() {

    Detail::Initialize(feature_transformer);
    Detail::Initialize(network);
  }

  // Read network header
  bool ReadHeader(std::istream& stream, std::uint32_t* hash_value, std::string* architecture)
  {
    std::uint32_t version, size;

    version     = read_little_endian<std::uint32_t>(stream);
    *hash_value = read_little_endian<std::uint32_t>(stream);
    size        = read_little_endian<std::uint32_t>(stream);
    if (!stream || version != kVersion) return false;
    architecture->resize(size);
    stream.read(&(*architecture)[0], size);
    return !stream.fail();
  }

  // Read network parameters
  bool ReadParameters(std::istream& stream) {

    std::uint32_t hash_value;
    std::string architecture;
    if (!ReadHeader(stream, &hash_value, &architecture)) return false;
    if (hash_value != kHashValue) return false;
    if (!Detail::ReadParameters(stream, *feature_transformer)) return false;
    if (!Detail::ReadParameters(stream, *network)) return false;
    return stream && stream.peek() == std::ios::traits_type::eof();
  }

  // Evaluation function. Perform differential calculation.
  Value evaluate(const Position& pos) {

    // We manually align the arrays on the stack because with gcc < 9.3
    // overaligning stack variables with alignas() doesn't work correctly.

    constexpr uint64_t alignment = kCacheLineSize;

#if defined(ALIGNAS_ON_STACK_VARIABLES_BROKEN)
    TransformedFeatureType transformed_features_unaligned[
      FeatureTransformer::kBufferSize + alignment / sizeof(TransformedFeatureType)];
    char buffer_unaligned[Network::kBufferSize + alignment];

    auto* transformed_features = align_ptr_up<alignment>(&transformed_features_unaligned[0]);
    auto* buffer = align_ptr_up<alignment>(&buffer_unaligned[0]);
#else
    alignas(alignment)
      TransformedFeatureType transformed_features[FeatureTransformer::kBufferSize];
    alignas(alignment) char buffer[Network::kBufferSize];
#endif

    ASSERT_ALIGNED(transformed_features, alignment);
    ASSERT_ALIGNED(buffer, alignment);

    feature_transformer->Transform(pos, transformed_features);
    const auto output = network->Propagate(transformed_features, buffer);

    return static_cast<Value>(output[0] / FV_SCALE);
  }

  // Load eval, from a file stream or a memory stream
  bool load_eval(std::string name, std::istream& stream) {

    Initialize();
    fileName = name;
    return ReadParameters(stream);
  }

} // namespace Eval::NNUE
