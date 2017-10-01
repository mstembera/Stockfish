/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include <algorithm>
#include <cassert>

#include "bitboard.h"
#include "pawns.h"
#include "position.h"
#include "thread.h"

int wm[50] = { 0 };
int we[50] = { 0 };

const int indexPairs[50][2] = {
  {1, 1458}, {3, 4374}, {27, 54}, {243, 486}, {9, 13122}, {81, 162}, {4, 5832},
  {82, 1620}, {495, 13365}, {4383, 13125}, {2, 729}, {6, 2187}, {1461, 4375},
  {165, 4455}, {18, 6561}, {1467, 13123}, {1476, 6562}, {30, 4428}, {13, 18954},
  {244, 1944}, {55, 1485}, {487, 1701}, {1464, 2188}, {247, 6318}, {91, 14742},
  {39, 17550}, {1623, 4456}, {4869, 13368}, {1638, 6643}, {60, 2214}, {5850, 6565},
  {1953, 13366}, {63, 13149}, {246, 4860}, {108, 216}, {5841, 13126}, {72, 6588},
  {10, 14580}, {4464, 13287}, {405, 567}, {90, 13284}, {163, 1539}, {492, 2430},
  {164, 810}, {270, 540}, {36, 13176}, {324, 648}, {12, 17496}, {489, 4617}, {488, 972} };


uint16_t base3Tbl[0x70708];
Score patternBonus[19683] = { SCORE_ZERO };

const Square corners3x3[12] = {
    SQ_A2, SQ_B2, SQ_C2,
    SQ_A3, SQ_B3, SQ_C3,
    SQ_A4, SQ_B4, SQ_C4,
    SQ_A5, SQ_B5, SQ_C5 };

uint8_t reverseBits[256];

inline uint8_t reverse_bits(uint8_t b)
{
    return uint8_t(((b * 0x80200802ULL) & 0x0884422110ULL) * 0x0101010101ULL >> 32);
}

Bitboard flip_files(Bitboard b)
{
    union { Bitboard bb; uint8_t u[8]; } v = { b };

    for (unsigned i = 0; i < 8; ++i)
        v.u[i] = reverseBits[v.u[i]];

    return v.bb;
}

int to_base3(uint32_t bits9)
{
    int value = 0;
    int digit = 1;

    for (unsigned i = 0; i < 9 && bits9; ++i)
    {
        value += (bits9 & 0x1) * digit;
        bits9 >>= 1;
        digit *= 3;
    }

    return value;
}

uint32_t get3x3_bits(Bitboard b, Square s)
{
    uint32_t bits = 0;
    if (b)
    {
        Rank r = rank_of(s);
        File f = file_of(s);
        assert(r < RANK_6 && f < FILE_D);

        int shift = (r - RANK_1) * 8 + (f - FILE_A);
        bits = uint32_t(b >> shift) & 0x70707;
    }
    return bits;
}


namespace {

  #define V Value
  #define S(mg, eg) make_score(mg, eg)

  // Isolated pawn penalty by opposed flag
  const Score Isolated[] = { S(27, 30), S(13, 18) };

  // Backward pawn penalty by opposed flag
  const Score Backward[] = { S(40, 26), S(24, 12) };

  // Connected pawn bonus by opposed, phalanx, #support and rank
  Score Connected[2][2][3][RANK_NB];

  // Doubled pawn penalty
  const Score Doubled = S(18, 38);

  // Lever bonus by rank
  const Score Lever[RANK_NB] = {
    S( 0,  0), S( 0,  0), S(0, 0), S(0, 0),
    S(17, 16), S(33, 32), S(0, 0), S(0, 0)
  };

  // Weakness of our pawn shelter in front of the king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where we have no pawns or our pawn is behind our king.
  const Value ShelterWeakness[][RANK_NB] = {
    { V(100), V(20), V(10), V(46), V(82), V( 86), V( 98) },
    { V(116), V( 4), V(28), V(87), V(94), V(108), V(104) },
    { V(109), V( 1), V(59), V(87), V(62), V( 91), V(116) },
    { V( 75), V(12), V(43), V(59), V(90), V( 84), V(112) }
  };

  // Danger of enemy pawns moving toward our king by [type][distance from edge][rank].
  // For the unopposed and unblocked cases, RANK_1 = 0 is used when opponent has no pawn
  // on the given file, or their pawn is behind our king.
  const Value StormDanger[][4][RANK_NB] = {
    { { V( 0),  V(-290), V(-274), V(57), V(41) },  //BlockedByKing
      { V( 0),  V(  60), V( 144), V(39), V(13) },
      { V( 0),  V(  65), V( 141), V(41), V(34) },
      { V( 0),  V(  53), V( 127), V(56), V(14) } },
    { { V( 4),  V(  73), V( 132), V(46), V(31) },  //Unopposed
      { V( 1),  V(  64), V( 143), V(26), V(13) },
      { V( 1),  V(  47), V( 110), V(44), V(24) },
      { V( 0),  V(  72), V( 127), V(50), V(31) } },
    { { V( 0),  V(   0), V(  79), V(23), V( 1) },  //BlockedByPawn
      { V( 0),  V(   0), V( 148), V(27), V( 2) },
      { V( 0),  V(   0), V( 161), V(16), V( 1) },
      { V( 0),  V(   0), V( 171), V(22), V(15) } },
    { { V(22),  V(  45), V( 104), V(62), V( 6) },  //Unblocked
      { V(31),  V(  30), V(  99), V(39), V(19) },
      { V(23),  V(  29), V(  96), V(41), V(15) },
      { V(21),  V(  23), V( 116), V(41), V(15) } }
  };

  // Max bonus for king safety. Corresponds to start position with all the pawns
  // in front of the king and no enemy pawn on the horizon.
  const Value MaxSafetyBonus = V(258);

  #undef S
  #undef V

  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {

    const Color  Them  = (Us == WHITE ? BLACK      : WHITE);
    const Square Up    = (Us == WHITE ? NORTH      : SOUTH);
    const Square Right = (Us == WHITE ? NORTH_EAST : SOUTH_WEST);
    const Square Left  = (Us == WHITE ? NORTH_WEST : SOUTH_EAST);

    Bitboard b, neighbours, stoppers, doubled, supported, phalanx;
    Bitboard lever, leverPush;
    Square s;
    bool opposed, backward;
    Score score = SCORE_ZERO;
    const Square* pl = pos.squares<PAWN>(Us);

    Bitboard ourPawns   = pos.pieces(  Us, PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    e->passedPawns[Us]   = e->pawnAttacksSpan[Us] = 0;
    e->semiopenFiles[Us] = 0xFF;
    e->kingSquares[Us]   = SQ_NONE;
    e->pawnAttacks[Us]   = shift<Right>(ourPawns) | shift<Left>(ourPawns);
    e->pawnsOnSquares[Us][BLACK] = popcount(ourPawns & DarkSquares);
    e->pawnsOnSquares[Us][WHITE] = pos.count<PAWN>(Us) - e->pawnsOnSquares[Us][BLACK];

    // Loop through all pawns of the current color and score each pawn
    while ((s = *pl++) != SQ_NONE)
    {
        assert(pos.piece_on(s) == make_piece(Us, PAWN));

        File f = file_of(s);

        e->semiopenFiles[Us]   &= ~(1 << f);
        e->pawnAttacksSpan[Us] |= pawn_attack_span(Us, s);

        // Flag the pawn
        opposed    = theirPawns & forward_file_bb(Us, s);
        stoppers   = theirPawns & passed_pawn_mask(Us, s);
        lever      = theirPawns & PawnAttacks[Us][s];
        leverPush  = theirPawns & PawnAttacks[Us][s + Up];
        doubled    = ourPawns   & (s - Up);
        neighbours = ourPawns   & adjacent_files_bb(f);
        phalanx    = neighbours & rank_bb(s);
        supported  = neighbours & rank_bb(s - Up);

        // A pawn is backward when it is behind all pawns of the same color on the
        // adjacent files and cannot be safely advanced.
        if (!neighbours || lever || relative_rank(Us, s) >= RANK_5)
            backward = false;
        else
        {
            // Find the backmost rank with neighbours or stoppers
            b = rank_bb(backmost_sq(Us, neighbours | stoppers));

            // The pawn is backward when it cannot safely progress to that rank:
            // either there is a stopper in the way on this rank, or there is a
            // stopper on adjacent file which controls the way to that rank.
            backward = (b | shift<Up>(b & adjacent_files_bb(f))) & stoppers;

            assert(!(backward && (forward_ranks_bb(Them, s + Up) & neighbours)));
        }

        // Passed pawns will be properly scored in evaluation because we need
        // full attack info to evaluate them. Include also not passed pawns
        // which could become passed after one or two pawn pushes when are
        // not attacked more times than defended.
        if (   !(stoppers ^ lever ^ leverPush)
            && !(ourPawns & forward_file_bb(Us, s))
            && popcount(supported) >= popcount(lever)
            && popcount(phalanx)   >= popcount(leverPush))
            e->passedPawns[Us] |= s;

        else if (   stoppers == SquareBB[s + Up]
                 && relative_rank(Us, s) >= RANK_5)
        {
            b = shift<Up>(supported) & ~theirPawns;
            while (b)
                if (!more_than_one(theirPawns & PawnAttacks[Us][pop_lsb(&b)]))
                    e->passedPawns[Us] |= s;
        }

        // Score this pawn
        if (supported | phalanx)
            score += Connected[opposed][!!phalanx][popcount(supported)][relative_rank(Us, s)];

        else if (!neighbours)
            score -= Isolated[opposed];

        else if (backward)
            score -= Backward[opposed];

        if (doubled && !supported)
            score -= Doubled;

        if (lever)
            score += Lever[relative_rank(Us, s)];
    }

    return score;
  }

} // namespace

namespace Pawns {

/// Pawns::init() initializes some tables needed by evaluation. Instead of using
/// hard-coded tables, when makes sense, we prefer to calculate them with a formula
/// to reduce independent parameters and to allow easier tuning and better insight.

void init() {

    for (unsigned i = 0; i < 256; ++i)
        reverseBits[i] = reverse_bits((uint8_t)i);

    for (unsigned i = 0; i <= 0x70707; ++i)
    {
        uint32_t bits3x3 = (i & 0x7) | ((i & 0x700) >> 5) | ((i & 0x70000) >> 10);
        base3Tbl[i] = to_base3(bits3x3);
    }

  static const int Seed[RANK_NB] = { 0, 13, 24, 18, 76, 100, 175, 330 };

  for (int opposed = 0; opposed <= 1; ++opposed)
      for (int phalanx = 0; phalanx <= 1; ++phalanx)
          for (int support = 0; support <= 2; ++support)
              for (Rank r = RANK_2; r < RANK_8; ++r)
  {
      int v = 17 * support;
      v += (Seed[r] + (phalanx ? (Seed[r + 1] - Seed[r]) / 2 : 0)) >> opposed;

      Connected[opposed][phalanx][support][r] = make_score(v, v * (r - 2) / 4);
  }
}


/// Pawns::probe() looks up the current position's pawns configuration in
/// the pawns hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same pawns configuration occurs again.

Entry* probe(const Position& pos) {

  Key key = pos.pawn_key();
  Entry* e = pos.this_thread()->pawnsTable[key];

  if (e->key == key)
      return e;

  e->key = key;
  e->score = evaluate<WHITE>(pos, e) - evaluate<BLACK>(pos, e);

  Bitboard wPawnsL = pos.pieces(WHITE, PAWN);
  Bitboard bPawnsL = pos.pieces(BLACK, PAWN);
  Bitboard wPawnsR = flip_files(wPawnsL);
  Bitboard bPawnsR = flip_files(bPawnsL);

  for (int i = 0; i < 12; ++i)
  {
      Square corner = corners3x3[i];

      int idxL = base3Tbl[get3x3_bits(wPawnsL, corner)] + base3Tbl[get3x3_bits(bPawnsL, corner)] * 2;
      int idxR = base3Tbl[get3x3_bits(wPawnsR, corner)] + base3Tbl[get3x3_bits(bPawnsR, corner)] * 2;
      assert(idxL < 19683 && idxR < 19683);
      e->score += patternBonus[idxL] + patternBonus[idxR];
  }

  e->asymmetry = popcount(e->semiopenFiles[WHITE] ^ e->semiopenFiles[BLACK]);
  e->openFiles = popcount(e->semiopenFiles[WHITE] & e->semiopenFiles[BLACK]);
  return e;
}


/// Entry::shelter_storm() calculates shelter and storm penalties for the file
/// the king is on, as well as the two closest files.

template<Color Us>
Value Entry::shelter_storm(const Position& pos, Square ksq) {

  const Color Them = (Us == WHITE ? BLACK : WHITE);

  enum { BlockedByKing, Unopposed, BlockedByPawn, Unblocked };

  Bitboard b = pos.pieces(PAWN) & (forward_ranks_bb(Us, ksq) | rank_bb(ksq));
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);
  Value safety = MaxSafetyBonus;
  File center = std::max(FILE_B, std::min(FILE_G, file_of(ksq)));

  for (File f = center - File(1); f <= center + File(1); ++f)
  {
      b = ourPawns & file_bb(f);
      Rank rkUs = b ? relative_rank(Us, backmost_sq(Us, b)) : RANK_1;

      b = theirPawns & file_bb(f);
      Rank rkThem = b ? relative_rank(Us, frontmost_sq(Them, b)) : RANK_1;

      int d = std::min(f, FILE_H - f);
      safety -=  ShelterWeakness[d][rkUs]
               + StormDanger
                 [f == file_of(ksq) && rkThem == relative_rank(Us, ksq) + 1 ? BlockedByKing  :
                  rkUs   == RANK_1                                          ? Unopposed :
                  rkThem == rkUs + 1                                        ? BlockedByPawn  : Unblocked]
                 [d][rkThem];
  }

  return safety;
}


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total king_safety() calls.

template<Color Us>
Score Entry::do_king_safety(const Position& pos, Square ksq) {

  kingSquares[Us] = ksq;
  castlingRights[Us] = pos.can_castle(Us);
  int minKingPawnDistance = 0;

  Bitboard pawns = pos.pieces(Us, PAWN);
  if (pawns)
      while (!(DistanceRingBB[ksq][minKingPawnDistance++] & pawns)) {}

  Value bonus = shelter_storm<Us>(pos, ksq);

  // If we can castle use the bonus after the castling if it is bigger
  if (pos.can_castle(MakeCastling<Us, KING_SIDE>::right))
      bonus = std::max(bonus, shelter_storm<Us>(pos, relative_square(Us, SQ_G1)));

  if (pos.can_castle(MakeCastling<Us, QUEEN_SIDE>::right))
      bonus = std::max(bonus, shelter_storm<Us>(pos, relative_square(Us, SQ_C1)));

  return make_score(bonus, -16 * minKingPawnDistance);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos, Square ksq);
template Score Entry::do_king_safety<BLACK>(const Position& pos, Square ksq);

} // namespace Pawns

UPDATE_ON_LAST();

void my_post_update()
{
    for (int i = 0; i < 50; ++i)
    {
        patternBonus[indexPairs[i][0]] = make_score( wm[i],  we[i]);
        patternBonus[indexPairs[i][1]] = make_score(-wm[i], -we[i]);
    }
}

TUNE(SetRange(-40, 40), wm, we, my_post_update);

