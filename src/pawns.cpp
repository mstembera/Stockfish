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
    S( 10,  12), S( 13,   2), S( 19,   3), S(  2,   4), S( 24,  -3), S( -1,  -4), S( -3,  -6), S(  2,  -5),
    S( -9,  18), S( -8,   6), S( -4,  15), S(-16,   0), S( -6,  17), S(-11,  -7), S(-27, -19), S(-11,  -1),
    S(  7,   4), S( 10, -14), S(-17,   8), S( 20,   3), S(  1,  -5), S(-35,   2), S(  2,  -5), S(-24,  12),
    S( 21,   8), S( 17, -20), S(-13, -25), S( -8, -15), S( -1,  35), S( 12,   6), S( -5,   6), S(-30,  -9),
    S( -4,  10), S( 24, -12), S( 24,  33), S(  5,   2), S(  0,  18), S( -1, -12), S( -8, -36), S( 15,   2),
    S( 12,  35), S(  9,  15), S( -8,  -6), S( -1,   6), S(  1,   5), S(-11,  26), S(  3,  11), S( -4, -16),
    S(-14, -20), S(-29,  -3), S(-10,  -3), S(-16,   3), S( 20, -11), S(  7,  11), S( 13, -10), S( 10,  -3),
    S( 12,  16), S( 26,  23), S( 10,   1), S( 35,  -1), S(-21,  10), S(-16,   8), S(  6, -24), S(  7,   6),
    S( -6,  -8), S(  2,  19), S(  3,  -4), S(  5, -24), S( -8, -15), S(-21,  -9), S( 19,   1), S( 19, -18),
    S( 27,   0), S(  9,  -9), S( 31, -17), S( -2,  22), S( 11, -27), S(-16,  -9), S( -2, -15), S(-20,   6),
    S( 22,  -7), S( 20,   9), S( 25,  14), S(-22, -31), S(  2,  11), S( 26,  -9), S(  8,  20), S(-16,  16),
    S( -7,   7), S( 27,  -4), S( 11,  10), S(  6, -17), S(-14,  10), S( 26,  -7), S(-29,   2), S(-18,  15),
    S(-13,  -2), S( -3,   2), S(  5,  -3), S( -7,  -5), S(-39, -27), S(  1,  25), S(-17,   9), S(-26,  -3),
    S( -4, -14), S(  0, -11), S( 25,  -2), S(-18, -19), S( 16, -10), S( 10,  16), S( 23,  -4), S(  0, -22),
    S(-17,   2), S( 37,   7), S(-26,  33), S(  2,   7), S(  6,   2), S(  3,   0), S(-17, -15), S(  3,  -2),
    S( 33, -28), S(-16,  -6), S( -5,  -1), S(  6, -25), S(-11,   6), S( -4,  -2), S(-14,  17), S(-24,   2),
    S(-15, -18), S( 15,  21), S(-37,  24), S( -5, -21), S( -4, -11), S( -7,   9), S(-30,   8), S( 28,  -8),
    S( -3,  -9), S(  6,   0), S(-21, -17), S(-19,  30), S( -8, -25), S( 11,   7), S( 46,   5), S( 23, -11),
    S(-26,   4), S( 16, -19), S(-20,  16), S(-13, -17), S( 20,  23), S( 30, -27), S(  6,  26), S(-32,  -6),
    S( 23, -26), S( -8,  20), S(  3,  -4), S( 30,  19), S( 24,  -5), S( 11,  -8), S(-24,   9), S( 13, -27),
    S( 14,   2), S( 38,  -7), S( -4,   6), S( -2,  18), S(-17,  11), S( 19,   7), S(  3, -11), S( -4, -11),
    S( -1,   4), S( 11,  13), S(-31,  13), S( 28,   6), S( 15,  -8), S( -8,   6), S(-15,   4), S(  5,  -8),
    S( 35, -13), S( 19,   3), S(  3,  24), S( -1,  14), S( -1,  -5), S(-14,  -3), S(  5,  -8), S( 19, -23),
    S(-16, -19), S( 26,  15), S( 18,  16), S(-18,  11), S(-18, -23), S( -5,   4), S(-10,  21), S( -2,  -2),
    S(-10,  18), S(-15,   0), S( 20, -27), S(-10,  -5), S( 19,  -2), S( 28,  18), S(-19,  -1), S( -8,  -2),
    S( 21,  -6), S( 15,   4), S( 38,  22), S(-15,   1), S(  7,  21), S( 11,  -1), S(  5,   0), S( -3,   4),
    S( 23, -11), S( 16,   4), S(-25,   5), S(-10,   5), S(-19,   6), S(-29,   1), S(  5, -16), S( -5,   3),
    S( 17,  -3), S( -1, -14), S( 20, -28), S( 11,  36), S(-17,  -3), S(-22,   4), S(-14,  11), S(  8, -22),
    S(  7, -10), S( -5,   5), S(-31,  14), S( 11,  -2), S(  1,  33), S( -8, -33), S(  3,   5), S(  9, -13) };

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
