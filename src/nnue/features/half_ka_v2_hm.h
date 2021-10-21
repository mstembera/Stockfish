/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2021 The Stockfish developers (see AUTHORS file)

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

//Definition of input features HalfKP of NNUE evaluation function

#ifndef NNUE_FEATURES_HALF_KA_V2_HM_H_INCLUDED
#define NNUE_FEATURES_HALF_KA_V2_HM_H_INCLUDED

#include "../nnue_common.h"

#include "../../evaluate.h"
#include "../../misc.h"

namespace Stockfish {
  struct StateInfo;
}

namespace Stockfish::Eval::NNUE::Features {

  // Feature HalfKAv2_hm: Combination of the position of own king
  // and the position of pieces. Position mirrored such that king always on e..h files.
  class HalfKAv2_hm {

    // unique number for each piece type on each square
    enum {
      PS_NONE     =  0,
      PS_W_PAWN   =  0,
      PS_B_PAWN   =  1 * SQUARE_NB,
      PS_W_KNIGHT =  2 * SQUARE_NB,
      PS_B_KNIGHT =  3 * SQUARE_NB,
      PS_W_BISHOP =  4 * SQUARE_NB,
      PS_B_BISHOP =  5 * SQUARE_NB,
      PS_W_ROOK   =  6 * SQUARE_NB,
      PS_B_ROOK   =  7 * SQUARE_NB,
      PS_W_QUEEN  =  8 * SQUARE_NB,
      PS_B_QUEEN  =  9 * SQUARE_NB,
      PS_KING     =  10 * SQUARE_NB,
      PS_NB = 11 * SQUARE_NB
    };

    static constexpr IndexType PieceSquareIndex[COLOR_NB][PIECE_NB] = {
      // convention: W - us, B - them
      // viewed from other side, W and B are reversed
      { PS_NONE, PS_W_PAWN, PS_W_KNIGHT, PS_W_BISHOP, PS_W_ROOK, PS_W_QUEEN, PS_KING, PS_NONE,
        PS_NONE, PS_B_PAWN, PS_B_KNIGHT, PS_B_BISHOP, PS_B_ROOK, PS_B_QUEEN, PS_KING, PS_NONE },
      { PS_NONE, PS_B_PAWN, PS_B_KNIGHT, PS_B_BISHOP, PS_B_ROOK, PS_B_QUEEN, PS_KING, PS_NONE,
        PS_NONE, PS_W_PAWN, PS_W_KNIGHT, PS_W_BISHOP, PS_W_ROOK, PS_W_QUEEN, PS_KING, PS_NONE }
    };

    // Orient a square according to perspective (rotates by 180 for black)
    static Square orient(Color perspective, Square s, Square ksq);

    // Index of a feature for a given king position and another piece on some square
    static IndexType make_index(Color perspective, Square s, Piece pc, Square ksq);

   public:
    // Feature name
    static constexpr const char* Name = "HalfKAv2_hm(Friend)";

    // Hash value embedded in the evaluation file
    static constexpr std::uint32_t HashValue = 0x7f234cb8u;

    // Number of feature dimensions
    static constexpr IndexType Dimensions =
        static_cast<IndexType>(SQUARE_NB) * static_cast<IndexType>(PS_NB) / 2;

#define P(v) (v * PS_NB)
    static constexpr int KingBuckets[COLOR_NB][SQUARE_NB] = {
      { P(28), P(29), P(30), P(31), P(31), P(30), P(29), P(28),
        P(24), P(25), P(26), P(27), P(27), P(26), P(25), P(24),
        P(20), P(21), P(22), P(23), P(23), P(22), P(21), P(20),
        P(16), P(17), P(18), P(19), P(19), P(18), P(17), P(16),
        P(12), P(13), P(14), P(15), P(15), P(14), P(13), P(12),
        P( 8), P( 9), P(10), P(11), P(11), P(10), P( 9), P( 8),
        P( 4), P( 5), P( 6), P( 7), P( 7), P( 6), P( 5), P( 4),
        P( 0), P( 1), P( 2), P( 3), P( 3), P( 2), P( 1), P( 0) },
      { P( 0), P( 1), P( 2), P( 3), P( 3), P( 2), P( 1), P( 0),
        P( 4), P( 5), P( 6), P( 7), P( 7), P( 6), P( 5), P( 4),
        P( 8), P( 9), P(10), P(11), P(11), P(10), P( 9), P( 8),
        P(12), P(13), P(14), P(15), P(15), P(14), P(13), P(12),
        P(16), P(17), P(18), P(19), P(19), P(18), P(17), P(16),
        P(20), P(21), P(22), P(23), P(23), P(22), P(21), P(20),
        P(24), P(25), P(26), P(27), P(27), P(26), P(25), P(24),
        P(28), P(29), P(30), P(31), P(31), P(30), P(29), P(28) }
    };
#undef P

    // Orient a square according to perspective (rotates by 180 for black)
    static constexpr int OrientTBL[COLOR_NB][SQUARE_NB] = {
      { SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1,
        SQ_H1, SQ_H1, SQ_H1, SQ_H1, SQ_A1, SQ_A1, SQ_A1, SQ_A1 },
      { SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8,
        SQ_H8, SQ_H8, SQ_H8, SQ_H8, SQ_A8, SQ_A8, SQ_A8, SQ_A8 }
    };

    // Maximum number of simultaneously active features.
    static constexpr IndexType MaxActiveDimensions = 32;

    // Get a list of indices for active features
    static void append_active_indices(
      const Position& pos,
      Color perspective,
      ValueListInserter<IndexType> active);

    // Get a list of indices for recently changed features
    static void append_changed_indices(
      Square ksq,
      StateInfo* st,
      Color perspective,
      ValueListInserter<IndexType> removed,
      ValueListInserter<IndexType> added);

    // Returns the cost of updating one perspective, the most costly one.
    // Assumes no refresh needed.
    static int update_cost(StateInfo* st);
    static int refresh_cost(const Position& pos);

    // Returns whether the change stored in this StateInfo means that
    // a full accumulator refresh is required.
    static bool requires_refresh(StateInfo* st, Color perspective);
  };

}  // namespace Stockfish::Eval::NNUE::Features

#endif // #ifndef NNUE_FEATURES_HALF_KA_V2_HM_H_INCLUDED
