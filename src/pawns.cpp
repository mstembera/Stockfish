/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2018 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

namespace {

  #define V Value
  #define S(mg, eg) make_score(mg, eg)

  // Pawn penalties
  constexpr Score Backward = S( 9, 24);
  constexpr Score Doubled  = S(11, 56);
  constexpr Score Isolated = S( 5, 15);

  // Connected pawn bonus by opposed, phalanx, #support and rank
  Score Connected[2][2][3][RANK_NB];

  constexpr Score Formation[256] = {
    S(  0,   6), S(  5, -12), S( 25,  -8), S(  0,  -4), S( 26,   8), S(-12,  -2), S( -2,  -6), S( -2,  -8),
    S(-10,   3), S(-17,   7), S(-12,  26), S(-10,  -2), S( -6,  12), S( -8, -22), S(-34, -18), S( -2,  21),
    S( 15,  -3), S( 16,  -6), S( -6,  15), S( 26,  -4), S(-28,  -8), S(-37,   6), S(  7,  -8), S( -8,  -6),
    S(  8,  17), S( 10, -20), S( -9, -15), S( -2, -12), S(  6,  37), S(  8,  18), S(  1,   8), S(-15,  -5),
    S( -5,   7), S( 19, -29), S( 21,  35), S( -5,  14), S(  8,  20), S( -1,  -4), S(-20, -42), S(  8,  -6),
    S( 14,  37), S( 15,  16), S(-19,   5), S( -4,  18), S( -1, -14), S( -8,  21), S(-11,  10), S(  0,   6),
    S(-21, -16), S(-17, -12), S(-11,  -4), S(-25, -11), S( 23,   2), S(  4,   6), S(  7, -21), S(  5,   2),
    S( 10,   5), S( 23,  31), S( -3,   8), S( 26,  -4), S(-20,   6), S(-19,   9), S(  6, -11), S( 21,  17),
    S( -7,  -2), S( 10,   7), S(  8,  -2), S( -2, -24), S( -7, -20), S( -5,  -1), S( 21,   8), S(  6, -38),
    S( 31,  -7), S( 13,  -5), S( 35, -19), S(-22,  26), S( 15, -20), S(-25,   3), S(-14, -22), S(-27,   5),
    S( 23, -10), S( 16,   0), S( 21,   1), S(-17, -37), S(  2,   7), S( 27,   3), S( 12,  27), S(-19,  17),
    S(  5,  -9), S( 22, -10), S( 17,  16), S( 10, -12), S(-33,   1), S( 25,  -5), S(-29,   0), S(-21,   7),
    S(-28,  -6), S( -9,   3), S( -6,  -5), S(-11,   1), S(-50, -33), S(  5,  23), S(-18,  26), S(-24,  -5),
    S( -4, -10), S(  4, -15), S( 24,   6), S(-19, -11), S( 25, -11), S( 14,  12), S( 20,  -5), S( 10, -20),
    S(-16,  -2), S( 22,  24), S(-40,  35), S( -9,   1), S( 14,   3), S( -9,   0), S(-22, -25), S( -1,  -5),
    S( 36, -27), S(-19,   0), S(-10,  11), S(  8, -17), S( -7,   2), S(-10,   7), S( -9,  19), S(-34,  -4),
    S( -6, -11), S( 18,  26), S(-30,  23), S(  2, -23), S(  6,  -9), S(-17,  -2), S(-24,   2), S( 42,  -6),
    S( -6, -21), S( 21, -12), S(-34, -22), S(-15,  32), S(-12, -24), S(  7, -10), S( 33,   8), S( 31, -21),
    S(-34,   2), S( 10, -13), S(-12,  11), S(-16, -11), S( 15,  15), S( 35, -26), S( 14,  18), S(-42, -20),
    S( 19, -33), S(-12,  13), S( 10,  14), S( 26,  18), S( 11,  -1), S( 15, -10), S(-27,   5), S( -1, -26),
    S( 10,  -1), S( 38,  -3), S(  4,   5), S( -6,  12), S(-17,   6), S( 26,   4), S( -4,  -4), S( -6,  -5),
    S(-12,  22), S(  3,  19), S(-34,  11), S( 28,  19), S( 15, -23), S( -9,  11), S( -7,  -2), S(  2,  -3),
    S( 26,  -8), S( 32,  -5), S( -5,  33), S( -5,  22), S( 11, -17), S(-13, -15), S( -2,  -8), S( 21, -25),
    S( -4, -16), S( 31,  26), S( 13,  29), S(-18,  13), S( -8, -18), S(  3,  13), S(-19,  17), S( -6,  -3),
    S(-10,  27), S(-15,  -1), S( 19, -23), S(-11,  16), S( 19,   4), S( 32,  13), S(-25,  11), S(  0,  -9),
    S( -9,   2), S(  8,  -1), S( 38,  28), S( -5,   8), S( 13,  13), S( 18,  -3), S(  3,   1), S(-11,   7),
    S( 25, -15), S( 28,   6), S(-10,  13), S(-13,  20), S(-31,  11), S(-34, -16), S(  4, -25), S( -4,  -8),
    S( 10,  -9), S(  9, -13), S( 22, -28), S(  3,  37), S(-15,   0), S(-13,   7), S(-11,  16), S( 10, -11),
    S( 10, -20), S(  9,   7), S(-36,   9), S( 10,   2), S( -1,  22), S(  4, -25), S( -2,   8), S(  3, -12) };

  // Strength of pawn shelter for our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
  constexpr Value ShelterStrength[int(FILE_NB) / 2][RANK_NB] = {
    { V( -6), V( 81), V( 93), V( 58), V( 39), V( 18), V(  25) },
    { V(-43), V( 61), V( 35), V(-49), V(-29), V(-11), V( -63) },
    { V(-10), V( 75), V( 23), V( -2), V( 32), V(  3), V( -45) },
    { V(-39), V(-13), V(-29), V(-52), V(-48), V(-67), V(-166) }
  };

  // Danger of enemy pawns moving toward our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
  // is behind our king.
  constexpr Value UnblockedStorm[int(FILE_NB) / 2][RANK_NB] = {
    { V( 89), V(107), V(123), V(93), V(57), V( 45), V( 51) },
    { V( 44), V(-18), V(123), V(46), V(39), V( -7), V( 23) },
    { V(  4), V( 52), V(162), V(37), V( 7), V(-14), V( -2) },
    { V(-10), V(-14), V( 90), V(15), V( 2), V( -7), V(-16) }
  };

  // Danger of blocked enemy pawns storming our king, by rank
  constexpr Value BlockedStorm[RANK_NB] =
    { V(0), V(0), V(66), V(6), V(5), V(1), V(15) };

  #undef S
  #undef V

  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {

    constexpr Color     Them = (Us == WHITE ? BLACK : WHITE);
    constexpr Direction Up   = (Us == WHITE ? NORTH : SOUTH);

    Bitboard b, neighbours, stoppers, doubled, supported, phalanx;
    Bitboard lever, leverPush;
    Square s;
    bool opposed, backward;
    Score score = SCORE_ZERO;
    const Square* pl = pos.squares<PAWN>(Us);

    Bitboard ourPawns   = pos.pieces(  Us, PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    e->passedPawns[Us] = e->pawnAttacksSpan[Us] = e->weakUnopposed[Us] = 0;
    e->semiopenFiles[Us] = 0xFF;
    e->kingSquares[Us]   = SQ_NONE;
    e->pawnAttacks[Us]   = pawn_attacks_bb<Us>(ourPawns);
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

        // A pawn is backward when it is behind all pawns of the same color
        // on the adjacent files and cannot be safely advanced.
        backward =  !(ourPawns & pawn_attack_span(Them, s + Up))
                  && (stoppers & (leverPush | (s + Up)));

        // Passed pawns will be properly scored in evaluation because we need
        // full attack info to evaluate them. Include also not passed pawns
        // which could become passed after one or two pawn pushes when are
        // not attacked more times than defended.
        if (   !(stoppers ^ lever ^ leverPush)
            && popcount(supported) >= popcount(lever) - 1
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
            score += Connected[opposed][bool(phalanx)][popcount(supported)][relative_rank(Us, s)];

        else if (!neighbours)
            score -= Isolated, e->weakUnopposed[Us] += !opposed;

        else if (backward)
            score -= Backward, e->weakUnopposed[Us] += !opposed;

        if (doubled && !supported)
            score -= Doubled;
    }

    score += Formation[e->semiopenFiles[Us] ^ 0xFF];

    return score;
  }

} // namespace

namespace Pawns {

/// Pawns::init() initializes some tables needed by evaluation. Instead of using
/// hard-coded tables, when makes sense, we prefer to calculate them with a formula
/// to reduce independent parameters and to allow easier tuning and better insight.

void init() {

  static constexpr int Seed[RANK_NB] = { 0, 13, 24, 18, 65, 100, 175, 330 };

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
  e->scores[WHITE] = evaluate<WHITE>(pos, e);
  e->scores[BLACK] = evaluate<BLACK>(pos, e);
  e->openFiles = popcount(e->semiopenFiles[WHITE] & e->semiopenFiles[BLACK]);
  e->asymmetry = popcount(  (e->passedPawns[WHITE]   | e->passedPawns[BLACK])
                          | (e->semiopenFiles[WHITE] ^ e->semiopenFiles[BLACK]));

  return e;
}


/// Entry::evaluate_shelter() calculates the shelter bonus and the storm
/// penalty for a king, looking at the king file and the two closest files.

template<Color Us>
Value Entry::evaluate_shelter(const Position& pos, Square ksq) {

  constexpr Color     Them = (Us == WHITE ? BLACK : WHITE);
  constexpr Direction Down = (Us == WHITE ? SOUTH : NORTH);
  constexpr Bitboard  BlockRanks = (Us == WHITE ? Rank1BB | Rank2BB : Rank8BB | Rank7BB);

  Bitboard b = pos.pieces(PAWN) & ~forward_ranks_bb(Them, ksq);
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);

  Value safety = (shift<Down>(theirPawns) & (FileABB | FileHBB) & BlockRanks & ksq) ?
                 Value(374) : Value(5);

  File center = std::max(FILE_B, std::min(FILE_G, file_of(ksq)));
  for (File f = File(center - 1); f <= File(center + 1); ++f)
  {
      b = ourPawns & file_bb(f);
      int ourRank = b ? relative_rank(Us, backmost_sq(Us, b)) : 0;

      b = theirPawns & file_bb(f);
      int theirRank = b ? relative_rank(Us, frontmost_sq(Them, b)) : 0;

      int d = std::min(f, ~f);
      safety += ShelterStrength[d][ourRank];
      safety -= (ourRank && (ourRank == theirRank - 1)) ? BlockedStorm[theirRank]
                                                        : UnblockedStorm[d][theirRank];
  }

  return safety;
}


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total king_safety() calls.

template<Color Us>
Score Entry::do_king_safety(const Position& pos) {

  Square ksq = pos.square<KING>(Us);
  kingSquares[Us] = ksq;
  castlingRights[Us] = pos.can_castle(Us);
  int minKingPawnDistance = 0;

  Bitboard pawns = pos.pieces(Us, PAWN);
  if (pawns)
      while (!(DistanceRingBB[ksq][++minKingPawnDistance] & pawns)) {}

  Value bonus = evaluate_shelter<Us>(pos, ksq);

  // If we can castle use the bonus after the castling if it is bigger
  if (pos.can_castle(Us | KING_SIDE))
      bonus = std::max(bonus, evaluate_shelter<Us>(pos, relative_square(Us, SQ_G1)));

  if (pos.can_castle(Us | QUEEN_SIDE))
      bonus = std::max(bonus, evaluate_shelter<Us>(pos, relative_square(Us, SQ_C1)));

  return make_score(bonus, -16 * minKingPawnDistance);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos);
template Score Entry::do_king_safety<BLACK>(const Position& pos);

} // namespace Pawns
