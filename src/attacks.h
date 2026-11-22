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

#ifndef ATTACKS_H_INCLUDED
#define ATTACKS_H_INCLUDED

#include <cassert>
#include <array>
#include <initializer_list>
#include <utility>

#include "misc.h"
#include "types.h"
#include "bitboard.h"

#if defined(USE_AVX2)
    #include <immintrin.h>
#elif defined(__wasm_simd128__)
    #include <wasm_simd128.h>
#endif
#ifdef _MSC_VER
    #include <cstdlib>  // _byteswap_uint64()
#endif

namespace Stockfish::Attacks {

void init();

inline Bitboard byteswap_bb(Bitboard b) {
#if defined(_MSC_VER) && !defined(__clang__)
    return _byteswap_uint64(b);
#else
    return __builtin_bswap64(b);
#endif
}

// Sliding attacks by hyperbola quintessence,
// see https://www.chessprogramming.org/Hyperbola_Quintessence
struct alignas(32) DualMagic {
    Bitboard maskDiag, maskAntidiag, maskFile, square;
};

struct alignas(64) DualMagicTable {
    DualMagic masks[SQUARE_NB];
    u8        rankAttacks[64 * FILE_NB];
};

extern const DualMagicTable DualMagics;

// We always compute [bishop, rook] attacks at once, then rely on
// compiler's DCE and CSE to eliminate unneeded re-computations or extractions.
//
// File, diagonal and antidiagonal attacks can use a byte reversal because all
// squares reside in different bytes. Rank attacks instead use the six inner
// occupancy bits (edge blockers never change the attack set).
sf_always_inline std::pair<Bitboard, Bitboard> both_attacks_bb(Square s, Bitboard occupied) {
    const DualMagic& m = DualMagics.masks[s];
    // Masking in place leaves occ6 * 2, hence the scale of 4 rather than 8.
    const int      shift = int(s) & 56;
    const Bitboard rank  = DualMagics.rankAttacks[((occupied >> shift) & 0x7e) * 4 + file_of(s)];
    const Bitboard rankAttacks = rank << shift;

#ifdef USE_AVX2
    // Byteswap within each 64-bit lane
    const auto bswap = [](__m256i v) {
        return _mm256_shuffle_epi8(v, _mm256_set_epi8(
          8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7,
          8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7));
    };

    // Each lane contains a mask and we follow the scalar algorithm below
    const __m256i mask = _mm256_load_si256(reinterpret_cast<const __m256i*>(&m));
    const __m256i rs   = _mm256_set1_epi64x(m.square);
    const __m256i rrs  = bswap(rs);

    const __m256i o      = _mm256_and_si256(mask, _mm256_set1_epi64x(occupied));
    const __m256i fwd    = _mm256_sub_epi64(o, rs);
    const __m256i rev    = bswap(_mm256_sub_epi64(bswap(o), rrs));
    const __m256i result = _mm256_and_si256(_mm256_xor_si256(fwd, rev), mask);

    const __m128i diagonals = _mm256_castsi256_si128(result);
    const __m128i bishop    = _mm_or_si128(diagonals, _mm_srli_si128(diagonals, 8));
    const __m128i file      = _mm256_extracti128_si256(result, 1);
    return {_mm_cvtsi128_si64(bishop), _mm_cvtsi128_si64(file) | rankAttacks};
#elif defined(__wasm_simd128__)
    // Byteswap within each 64-bit lane
    const auto bswap = [](v128_t v) {
        return wasm_i8x16_shuffle(v, v, 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8);
    };

    const v128_t occ = wasm_i64x2_splat(i64(occupied));
    const v128_t rs  = wasm_i64x2_splat(i64(m.square));
    const v128_t rrs = bswap(rs);

    // Two lanes of the scalar algorithm below
    const auto hyperbola2 = [&](v128_t mask) {
        const v128_t o   = wasm_v128_and(mask, occ);
        const v128_t fwd = wasm_i64x2_sub(o, rs);
        const v128_t rev = bswap(wasm_i64x2_sub(bswap(o), rrs));
        return wasm_v128_and(wasm_v128_xor(fwd, rev), mask);
    };

    // Bishop: [diagonal, antidiagonal]; rook: [file, none]. A query for one
    // piece type then computes only one vector.
    const v128_t diags = hyperbola2(wasm_v128_load(&m.maskDiag));
    const v128_t file  = hyperbola2(wasm_v128_load64_zero(&m.maskFile));
    return {Bitboard(wasm_i64x2_extract_lane(diags, 0) | wasm_i64x2_extract_lane(diags, 1)),
            Bitboard(wasm_i64x2_extract_lane(file, 0)) | rankAttacks};
#else
    const Bitboard rr = byteswap_bb(m.square);
    const auto hyperbola = [&](Bitboard mask) {
        const Bitboard o = occupied & mask;
        return ((o - m.square) ^ byteswap_bb(byteswap_bb(o) - rr)) & mask;
    };

    return {hyperbola(m.maskDiag) | hyperbola(m.maskAntidiag), hyperbola(m.maskFile) | rankAttacks};
#endif
}

extern Bitboard LineBB[SQUARE_NB][SQUARE_NB];
extern Bitboard BetweenBB[SQUARE_NB][SQUARE_NB];
extern Bitboard inverse_RayPassBB[SQUARE_NB][SQUARE_NB];
extern Bitboard DiagBB[SQUARE_NB];
extern Bitboard AntiDiagBB[SQUARE_NB];

// horizontal/vertical/diagonal line passing through s1 and s2
inline Bitboard line_bb(Square s1, Square s2) {
    assert(is_ok(s1) && is_ok(s2));
    return LineBB[s1][s2];
}

// horizontal/vertical/diagonal line segment from s1 to s2,
// excluding s1 but including s2
inline Bitboard between_bb(Square s1, Square s2) {
    assert(is_ok(s1) && is_ok(s2));
    return BetweenBB[s1][s2];
}

// ray going from s1 to s2 and past, excluding s1
inline Bitboard ray_pass_bb(Square s1, Square s2) {
    assert(is_ok(s1) && is_ok(s2));
    return inverse_RayPassBB[s2][s1];
}

inline Bitboard diag_bb(Square s) {
    assert(is_ok(s));
    return DiagBB[s];
}

inline Bitboard antidiag_bb(Square s) {
    assert(is_ok(s));
    return AntiDiagBB[s];
}

// Returns the bitboard of target square for the given step
// from the given square. If the step is off the board, returns empty bitboard.
constexpr Bitboard safe_destination(Square s, int step) {
    constexpr auto abs = [](int v) { return v < 0 ? -v : v; };
    Square         to  = Square(s + step);
    return is_ok(to) && abs(file_of(s) - file_of(to)) <= 2 ? square_bb(to) : Bitboard(0);
}

constexpr Bitboard sliding_attack(PieceType pt, Square sq, Bitboard occupied) {
    Bitboard            attacks             = 0;
    constexpr Direction RookDirections[4]   = {NORTH, SOUTH, EAST, WEST};
    constexpr Direction BishopDirections[4] = {NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST};

    for (Direction d : (pt == ROOK ? RookDirections : BishopDirections))
        for (Square s = sq; Bitboard dest = safe_destination(s, d); s += d)
        {
            attacks |= dest;
            if (occupied & dest)
                break;
        }

    return attacks;
}

constexpr Bitboard knight_attack(Square sq) {
    Bitboard b = {};
    for (int step : {-17, -15, -10, -6, 6, 10, 15, 17})
        b |= safe_destination(sq, step);
    return b;
}

constexpr Bitboard king_attack(Square sq) {
    Bitboard b = {};
    for (int step : {-9, -8, -7, -1, 1, 7, 8, 9})
        b |= safe_destination(sq, step);
    return b;
}

constexpr Bitboard pseudo_attacks(PieceType pt, Square sq) {
    switch (pt)
    {
    case PieceType::ROOK :
    case PieceType::BISHOP :
        return sliding_attack(pt, sq, 0);
    case PieceType::QUEEN :
        return sliding_attack(PieceType::ROOK, sq, 0) | sliding_attack(PieceType::BISHOP, sq, 0);
    case PieceType::KNIGHT :
        return knight_attack(sq);
    case PieceType::KING :
        return king_attack(sq);
    default :
        assert(false);
        return 0;
    }
}

inline constexpr auto PseudoAttacks = []() constexpr {
    std::array<std::array<Bitboard, SQUARE_NB>, PIECE_TYPE_NB> attacks{};

    for (Square s1 = SQ_A1; s1 <= SQ_H8; ++s1)
    {
        attacks[WHITE][s1] = pawn_attacks_bb<WHITE>(square_bb(s1));
        attacks[BLACK][s1] = pawn_attacks_bb<BLACK>(square_bb(s1));

        attacks[KING][s1]   = pseudo_attacks(KING, s1);
        attacks[KNIGHT][s1] = pseudo_attacks(KNIGHT, s1);
        attacks[QUEEN][s1] = attacks[BISHOP][s1] = pseudo_attacks(BISHOP, s1);
        attacks[QUEEN][s1] |= attacks[ROOK][s1]  = pseudo_attacks(ROOK, s1);
    }

    return attacks;
}();

// Returns the pseudo attacks of the given piece type
// assuming an empty board.
sf_always_inline Bitboard attacks_bb(PieceType pt, Square s, Color c = COLOR_NB) {

    assert((pt != PAWN || c < COLOR_NB) && is_ok(s));
    return pt == PAWN ? PseudoAttacks[c][s] : PseudoAttacks[pt][s];
}

// Returns the attacks by the given piece
// assuming the board is occupied according to the passed Bitboard.
// Sliding piece attacks do not continue past an occupied square.
sf_always_inline Bitboard attacks_bb(PieceType pt, Square s, Bitboard occupied) {

    assert(pt != PAWN && is_ok(s));

    [[maybe_unused]] const auto [bishop, rook] = both_attacks_bb(s, occupied);

    switch (pt)
    {
    case BISHOP :
        return bishop;
    case ROOK :
        return rook;
    case QUEEN :
        return bishop | rook;
    default :
        return PseudoAttacks[pt][s];
    }
}

}  // namespace Stockfish::Attacks

#endif  // #ifndef ATTACKS_H_INCLUDED
