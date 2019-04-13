/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include "types.h"

Value PieceValue[PHASE_NB][PIECE_NB] = {
  { VALUE_ZERO, PawnValueMg, KnightValueMg, BishopValueMg, RookValueMg, QueenValueMg },
  { VALUE_ZERO, PawnValueEg, KnightValueEg, BishopValueEg, RookValueEg, QueenValueEg }
};

namespace PSQT {

#define S(mg, eg) make_score(mg, eg)

// Bonus[PieceType][Square / 2] contains Piece-Square scores. For each piece
// type on a given square a (middlegame, endgame) score pair is assigned. Table
// is defined for files A..D and white side: it is symmetric for black side and
// second half of the files.
constexpr Score Bonus[][RANK_NB][int(FILE_NB) / 2] = {
  { },
  { },
  { // Knight
   { S(-166,-106), S(-92,-71), S(-78,-44), S(-75,-22) },
   { S(-81,  -72), S(-40,-53), S(-27,-14), S(-10,  2) },
   { S(-64,  -39), S(-19,-30), S(  4, -4), S( 20, 23) },
   { S(-29,  -37), S(  5,  4), S( 40, 15), S( 47, 31) },
   { S(-21,  -42), S( 22,-17), S( 49,  5), S( 61, 31) },
   { S(-13,  -48), S( 28,-29), S( 61,-10), S( 55, 20) },
   { S(-71,  -61), S(-23,-37), S(  1,-31), S( 34, 16) },
   { S(-199, -91), S(-78,-76), S(-53,-42), S(-30,-11) }
  },
  { // Bishop
   { S(-45,-65), S( -5,-32), S(-11,-29), S(-30, -9) },
   { S(-18,-41), S(  7,-16), S( 16, -9), S(  2, -2) },
   { S( -6,-20), S( 26, -2), S(  1, -1), S( 16, 12) },
   { S(  2,-29), S(  9, -7), S( 29,  5), S( 37, 13) },
   { S( -8,-27), S( 29, -9), S( 23, -5), S( 26, 15) },
   { S(-18,-33), S(  3, -6), S(  0,  1), S(  6, 10) },
   { S(-24,-37), S(-22,-22), S(  9, -3), S(-10,  6) },
   { S(-51,-57), S( -6,-46), S(-13,-38), S(-29,-25) }
  },
  { // Rook
   { S(-26,  0), S(-14, -9), S( -7, -3), S(  2, -3) },
   { S(-18, -4), S(-10, -6), S( -4,  5), S( 10,  2) },
   { S(-26, 18), S(-11, -2), S(  0,  7), S( -4,  2) },
   { S(-16,  0), S( -7,  2), S( -5, -5), S( -7,  9) },
   { S(-27, -6), S(-14,  3), S( -3,  4), S(  5,-10) },
   { S(-25,  4), S( -4, -6), S(  4,-11), S( 10,  5) },
   { S( -6,  1), S(  9, -3), S( 13, 15), S( 16,-12) },
   { S(-21, 12), S(-22,-10), S( -4, 12), S(  7,  4) }
  },
  { // Queen
   { S(  3,-70), S( -9,-57), S( -8,-49), S(  4,-26) },
   { S( -3,-52), S(  1,-26), S(  5,-20), S( 12,  1) },
   { S( -2,-38), S(  3,-16), S( 11, -9), S(  7,  6) },
   { S(  8,-18), S(  5,  3), S(  9, 17), S( 11, 31) },
   { S( -1,-29), S(  8, -5), S(  7,  8), S(  3, 23) },
   { S( -4,-41), S(  6,-19), S(  3,-16), S(  8,  0) },
   { S( -3,-49), S(  4,-24), S(  9,-24), S(  9, -5) },
   { S( -6,-79), S(-10,-54), S( -7,-48), S( -7,-38) }
  },
  { // King
   { S(273, -3), S(328, 40), S(275, 80), S(196, 91) },
   { S(273, 55), S(303, 98), S(238,138), S(184,131) },
   { S(197, 84), S(253,138), S(167,165), S(124,173) },
   { S(167,101), S(191,152), S(135,168), S(111,169) },
   { S(143, 96), S(176,166), S(111,197), S( 72,194) },
   { S(120, 87), S(159,165), S( 84,176), S( 39,189) },
   { S( 83, 36), S(118, 98), S( 61,127), S( 26,139) },
   { S( 62,  6), S( 87, 64), S( 48, 79), S(  3, 78) }
  }
};

constexpr Score PBonus[RANK_NB][FILE_NB] =
  { // Pawn (asymmetric distribution)
   { },
   { S(  4,-13), S( -5, -5), S( 10,  2), S( 17, -8), S( 21,  1), S( 20,  3), S( 10, -5), S(  0,-21) },
   { S(-13, -6), S(-16, -5), S(  8, -2), S( 20, -4), S( 20, -3), S( 25,  3), S(  2, -5), S(-26, -3) },
   { S( -6, -1), S(-19, -9), S(  6,-11), S( 24,-14), S( 32,-13), S( 26,-17), S( -2,-11), S(-14,-10) },
   { S(  8, 19), S( -4,  4), S(-12,  2), S(  3,-11), S( 11,-14), S(  7, -7), S(-10,  9), S(  3, 12) },
   { S( -5, 25), S(-10, 18), S(  2, 19), S( 12, 26), S(-16, 28), S(  0,  9), S(-11,  2), S(-13, 14) },
   { S(-12,  2), S(  0, -2), S(-12, 20), S(-13, 22), S( -8, 23), S(-17, 21), S( 10,  2), S( -3, 14) }
  };

#undef S

Score psq[PIECE_NB][SQUARE_NB];

// init() initializes piece-square tables: the white halves of the tables are
// copied from Bonus[] adding the piece value, then the black halves of the
// tables are initialized by flipping and changing the sign of the white scores.
void init() {

  for (Piece pc = W_PAWN; pc <= W_KING; ++pc)
  {
      PieceValue[MG][~pc] = PieceValue[MG][pc];
      PieceValue[EG][~pc] = PieceValue[EG][pc];

      Score score = make_score(PieceValue[MG][pc], PieceValue[EG][pc]);

      for (Square s = SQ_A1; s <= SQ_H8; ++s)
      {
          File f = std::min(file_of(s), ~file_of(s));
          psq[ pc][ s] = score + (type_of(pc) == PAWN ? PBonus[rank_of(s)][file_of(s)]
                                                      : Bonus[pc][rank_of(s)][f]);
          psq[~pc][~s] = -psq[pc][s];
      }
  }
}

} // namespace PSQT
