/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2026 The Stockfish developers (see AUTHORS file)

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

//Definition of input features FullThreats of NNUE evaluation function

#include "full_threats.h"

#include <array>
#include <cstdint>
#include <initializer_list>

#include "../../attacks.h"
#include "../../bitboard.h"
#include "../../misc.h"
#include "../../position.h"
#include "../../types.h"
#include "../nnue_common.h"

namespace Stockfish::Eval::NNUE::Features {

struct alignas(64) ThreatLUTs {
    // [attacker][attacked][from < to]
    std::array<std::array<std::array<u32, 2>, PIECE_NB>, PIECE_NB> lut1{};
    // [pt][from][to]
    std::array<std::array<std::array<u16, SQUARE_NB>, SQUARE_NB>, PIECE_TYPE_NB> lut2{};
};

constexpr std::array<Piece, 12> AllPieces = {
  W_PAWN, W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
  B_PAWN, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING,
};

constexpr auto init_threat_luts() {
    ThreatLUTs                     tables{};
    std::array<u16, PIECE_TYPE_NB> offsets{};

    for (PieceType pt : {PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING})
    {
        u16 offset = 0;
        for (Square from = SQ_A1; from <= SQ_H8; ++from)
        {
            for (Square to = SQ_A1; to <= SQ_H8; ++to)
            {
                Bitboard attacks = (pt != PAWN) ? Attacks::PseudoAttacks[pt][from]
                                 : (to < from)  ? Attacks::PseudoAttacks[BLACK][from]
                                                : Attacks::PseudoAttacks[WHITE][from];
                tables.lut2[pt][from][to] =
                  offset + constexpr_popcount(((1ULL << to) - 1) & attacks);
            }

            if (pt != PAWN)
                offset += constexpr_popcount(Attacks::PseudoAttacks[pt][from]);
            else if (from >= SQ_A2 && from <= SQ_H7)
                offset += constexpr_popcount(Attacks::PseudoAttacks[WHITE][from]);
        }

        offsets[pt] = offset;
    }

    IndexType cumulativeOffset = 0;
    for (Piece attacker : AllPieces)
    {
        const PieceType attackerType          = type_of(attacker);
        const IndexType cumulativePieceOffset = offsets[attackerType];
        const IndexType validTargets          = numValidTargets[attacker];

        for (Piece attacked : AllPieces)
        {
            bool      enemy        = (attacker ^ attacked) == 8;
            PieceType attackedType = type_of(attacked);

            int  map           = FullThreats::map[attackerType - 1][attackedType - 1];
            bool semi_excluded = attackerType == attackedType && (enemy || attackerType != PAWN);
            IndexType feature =
              cumulativeOffset
              + (color_of(attacked) * (validTargets / 2) + map) * cumulativePieceOffset;

            bool excluded                      = map < 0;
            tables.lut1[attacker][attacked][0] = excluded ? FullThreats::Dimensions : feature;
            tables.lut1[attacker][attacked][1] =
              excluded || semi_excluded ? FullThreats::Dimensions : feature;
        }

        cumulativeOffset += validTargets * cumulativePieceOffset;
    }

    return tables;
}

constexpr ThreatLUTs threat_luts = init_threat_luts();

// Index of a feature for a given king position and another piece on some square
sf_always_inline IndexType FullThreats::make_index(
  Color perspective, Piece attacker, Square from, Square to, Piece attacked, Square ksq) {
    const u8 orientation   = OrientTBL[ksq] ^ (56 * perspective);
    unsigned from_oriented = u8(from) ^ orientation;
    unsigned to_oriented   = u8(to) ^ orientation;

    const u8 swap              = 8 * perspective;
    unsigned attacker_oriented = attacker ^ swap;
    unsigned attacked_oriented = attacked ^ swap;

    return threat_luts.lut1[attacker_oriented][attacked_oriented][from_oriented < to_oriented]
         + threat_luts.lut2[type_of(attacker)][from_oriented][to_oriented];
}

// Get a list of indices for active features in ascending order

void FullThreats::append_active_indices(Color perspective, const Position& pos, IndexList& active) {
    const Square   ksq                = pos.square<KING>(perspective);
    const Bitboard occupied           = pos.pieces();
    const Bitboard pawnTargets        = pos.pieces(KNIGHT, ROOK);
    const Bitboard minorSliderTargets = pos.pieces(PAWN, KNIGHT, BISHOP, ROOK);
    const Bitboard queenTargets       = pos.pieces(PAWN, KNIGHT, BISHOP, ROOK, QUEEN);

    auto process_pawn_attacks = [&](Color c, Direction attkDir) {
        const Bitboard cPawns  = pos.pieces(c, PAWN);
        Bitboard       attacks = shift(cPawns, attkDir) & pawnTargets;
        while (attacks)
        {
            Square    to       = pop_lsb(attacks);
            Square    from     = to - attkDir;
            Piece     attacked = pos.piece_on(to);
            Piece     attacker = make_piece(c, PAWN);
            IndexType index    = make_index(perspective, attacker, from, to, attacked, ksq);
            active.push_back_if_lt(index, Dimensions);
        }
    };

    process_pawn_attacks(WHITE, NORTH_EAST);
    process_pawn_attacks(WHITE, NORTH_WEST);

    process_pawn_attacks(BLACK, SOUTH_WEST);
    process_pawn_attacks(BLACK, SOUTH_EAST);

    for (Color c : {WHITE, BLACK})
    {
        for (PieceType pt = KNIGHT; pt < KING; ++pt)
        {
            Piece    attacker = make_piece(c, pt);
            Bitboard bb       = pos.pieces(c, pt);
            Bitboard targets  = pt == KNIGHT || pt == QUEEN ? queenTargets : minorSliderTargets;
            while (bb)
            {
                Square   from    = pop_lsb(bb);
                Bitboard attacks = Attacks::attacks_bb(pt, from, occupied) & targets;
                while (attacks)
                {
                    Square    to       = pop_lsb(attacks);
                    Piece     attacked = pos.piece_on(to);
                    IndexType index    = make_index(perspective, attacker, from, to, attacked, ksq);
                    active.push_back_if_lt(index, Dimensions);
                }
            }
        }
    }
}

#ifdef USE_AVX512ICL
// Vectorized make_index() for up to 16 raw dirty threats. Lanes outside the mask
// are unspecified.
static sf_always_inline __m512i
make_indices(__m512i raw, __mmask16 lanes, unsigned orient, unsigned pairSwap) {
    static_assert(DirtyThreat::PcSqOffset == 0 && DirtyThreat::ThreatenedSqOffset == 8
                  && DirtyThreat::ThreatenedPcOffset == 16 && DirtyThreat::PcOffset == 20);
    // 32-bit gathers on the u16 lut2 read one entry too far. This stays in bounds
    // as the last piece type slot is unused.
    static_assert(KING < PIECE_TYPE_NB - 1);

    // A single XOR orients the squares and swaps the piece colors
    const __m512i oriented =
      _mm512_xor_si512(raw, _mm512_set1_epi32((orient * 0x101) | (pairSwap << 16)));

    const __m512i byteMask = _mm512_set1_epi32(0xff);
    const __m512i from     = _mm512_and_si512(oriented, byteMask);
    const __m512i to       = _mm512_and_si512(_mm512_srli_epi32(oriented, 8), byteMask);

    // lut1[pair][from < to], pair is bits 16-23
    const __m512i pair2 =
      _mm512_and_si512(_mm512_srli_epi32(oriented, 15), _mm512_set1_epi32(0x1fe));
    const __m512i idx1 = _mm512_mask_add_epi32(
      pair2, _mm512_cmplt_epu32_mask(from, to), pair2, _mm512_set1_epi32(1));

    // lut2[pt][from][to], pt is bits 20-22. Shares the shift with 'to'.
    const __m512i pt12 =
      _mm512_and_si512(_mm512_srli_epi32(oriented, 8), _mm512_set1_epi32(7 << 12));
    const __m512i idx2 =
      _mm512_ternarylogic_epi32(pt12, _mm512_slli_epi32(from, 6), to, 254 /* A | B | C */);

    const __m512i zero = _mm512_setzero_si512();
    const __m512i v1 =
      _mm512_mask_i32gather_epi32(zero, lanes, idx1, threat_luts.lut1[0][0].data(), 4);
    const __m512i v2 =
      _mm512_mask_i32gather_epi32(zero, lanes, idx2, threat_luts.lut2[0][0].data(), 2);

    return _mm512_add_epi32(v1, _mm512_and_si512(v2, _mm512_set1_epi32(0xffff)));
}

// Appends the selected indices in order
static sf_always_inline void
append_indices(FullThreats::IndexList& list, __mmask16 selected, __m256i indices) {
    // Avoid _mm256_mask_compressstoreu_epi16() as it's slow on Zen4. IndexList has
    // room for the 15 extra lanes of the unmasked store, see pp_3wide.cpp.
    _mm256_storeu_si256(reinterpret_cast<__m256i*>(list.make_space(popcount(selected))),
                        _mm256_maskz_compress_epi16(selected, indices));
}

template<usize N>
static void append_changed_indices_impl(const unsigned (&orient)[N],
                                        const unsigned (&pairSwap)[N],
                                        const FullThreats::DiffType& diff,
                                        FullThreats::IndexList* const (&removed)[N],
                                        FullThreats::IndexList* const (&added)[N],
                                        const ThreatWeightType* prefetchBase,
                                        IndexType               prefetchStride) {
    static_assert(sizeof(DirtyThreat) == 4);

    const DirtyThreat* dirties = diff.list.begin();
    const int          count   = diff.list.ssize();

    for (int i = 0; i < count; i += 16)
    {
        const int       n     = count - i < 16 ? count - i : 16;
        const __mmask16 lanes = __mmask16((1u << n) - 1);
        const __m512i   raw   = _mm512_maskz_loadu_epi32(lanes, dirties + i);
        const __mmask16 adds  = _mm512_movepi32_mask(raw);

        alignas(32) u16 unfiltered[N][16];

        for (usize p = 0; p < N; ++p)
        {
            const __m512i   index = make_indices(raw, lanes, orient[p], pairSwap[p]);
            const __mmask16 valid = _mm512_mask_cmplt_epu32_mask(
              lanes, index, _mm512_set1_epi32(FullThreats::Dimensions));
            const __m256i index16 = _mm512_cvtepi32_epi16(index);

            append_indices(*added[p], __mmask16(valid & adds), index16);
            append_indices(*removed[p], __mmask16(valid & ~adds), index16);
            _mm256_store_si256(reinterpret_cast<__m256i*>(unfiltered[p]), index16);
        }

        // Prefetch before filtering as the scalar path does: looping over the
        // filtered lists instead would add a hard to predict exit per list.
        if (prefetchBase)
            for (int k = 0; k < n; ++k)
                for (usize p = 0; p < N; ++p)
                    prefetch<PrefetchRw::READ, PrefetchLoc::LOW>(reinterpret_cast<const void*>(
                      reinterpret_cast<uintptr_t>(prefetchBase)
                      + uintptr_t(unfiltered[p][k]) * prefetchStride));
    }
}
#endif

// Get a list of indices for recently changed features

void FullThreats::append_changed_indices(Color                   perspective,
                                         Square                  ksq,
                                         const DiffType&         diff,
                                         IndexList&              removed,
                                         IndexList&              added,
                                         const ThreatWeightType* prefetchBase,
                                         IndexType               prefetchStride) {
    const unsigned orient    = OrientTBL[ksq] ^ (56 * perspective);
    const unsigned pair_swap = 0x88 * perspective;
#ifdef USE_AVX512ICL
    append_changed_indices_impl<1>({orient}, {pair_swap}, diff, {&removed}, {&added}, prefetchBase,
                                   prefetchStride);
#else
    const auto*    lut1_flat = &threat_luts.lut1[0][0];

    for (const auto& dirty : diff.list)
    {
        const u32      raw   = dirty.raw();
        const unsigned from  = u8(raw) ^ orient;
        const unsigned to    = u8(raw >> 8) ^ orient;
        const unsigned pair  = u8(raw >> 16);
        const auto&    plut2 = threat_luts.lut2[(pair >> 4) & 7];

        const IndexType index = lut1_flat[pair ^ pair_swap][from < to] + plut2[from][to];

        if (prefetchBase)
            prefetch<PrefetchRw::READ, PrefetchLoc::LOW>(reinterpret_cast<const void*>(
              reinterpret_cast<uintptr_t>(prefetchBase) + uintptr_t(index) * prefetchStride));

        if (raw >> 31)
            added.push_back_if_lt(index, Dimensions);
        else
            removed.push_back_if_lt(index, Dimensions);
    }
#endif
}

void FullThreats::append_changed_indices_both(Square                  white_ksq,
                                              Square                  black_ksq,
                                              const DiffType&         diff,
                                              IndexList&              white_removed,
                                              IndexList&              white_added,
                                              IndexList&              black_removed,
                                              IndexList&              black_added,
                                              const ThreatWeightType* prefetchBase,
                                              IndexType               prefetchStride) {
    const unsigned w_orient  = OrientTBL[white_ksq];
    const unsigned b_orient  = OrientTBL[black_ksq] ^ 56;
#ifdef USE_AVX512ICL
    append_changed_indices_impl<2>({w_orient, b_orient}, {0, 0x88}, diff,
                                   {&white_removed, &black_removed}, {&white_added, &black_added},
                                   prefetchBase, prefetchStride);
#else
    const auto*    lut1_flat = &threat_luts.lut1[0][0];

    for (const auto& dirty : diff.list)
    {
        const u32      raw   = dirty.raw();
        const unsigned from  = u8(raw);
        const unsigned to    = u8(raw >> 8);
        const unsigned pair  = u8(raw >> 16);
        const auto&    plut2 = threat_luts.lut2[(pair >> 4) & 7];

        const unsigned  w_from      = from ^ w_orient;
        const unsigned  w_to        = to ^ w_orient;
        const IndexType white_index = lut1_flat[pair][w_from < w_to] + plut2[w_from][w_to];

        const unsigned  b_from      = from ^ b_orient;
        const unsigned  b_to        = to ^ b_orient;
        const IndexType black_index = lut1_flat[pair ^ 0x88][b_from < b_to] + plut2[b_from][b_to];

        if (prefetchBase)
        {
            prefetch<PrefetchRw::READ, PrefetchLoc::LOW>(reinterpret_cast<const void*>(
              reinterpret_cast<uintptr_t>(prefetchBase) + uintptr_t(white_index) * prefetchStride));
            prefetch<PrefetchRw::READ, PrefetchLoc::LOW>(reinterpret_cast<const void*>(
              reinterpret_cast<uintptr_t>(prefetchBase) + uintptr_t(black_index) * prefetchStride));
        }

        if (raw >> 31)
        {
            white_added.push_back_if_lt(white_index, Dimensions);
            black_added.push_back_if_lt(black_index, Dimensions);
        }
        else
        {
            white_removed.push_back_if_lt(white_index, Dimensions);
            black_removed.push_back_if_lt(black_index, Dimensions);
        }
    }
#endif
}

}  // namespace Stockfish::Eval::NNUE::Features
