/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2022 The Stockfish developers (see AUTHORS file)

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

//Definition of input features HalfKAv2_hm of NNUE evaluation function

#include "half_ka_v2_hm.h"

#include "../../position.h"

namespace Stockfish::Eval::NNUE::Features {

  // Index of a feature for a given king position and another piece on some square
  inline IndexType HalfKAv2_hm::make_index(Color perspective, Square s, Piece pc, Square ksq) {
    return IndexType((int(s) ^ OrientTBL[perspective][ksq]) + PieceSquareIndex[perspective][pc] + KingBuckets[perspective][ksq]);
  }

  inline std::pair<IndexType, IndexType> HalfKAv2_hm::make_index_parts(Color perspective, Piece pc, Square ksq) {
    return std::pair(OrientTBL[perspective][ksq], PieceSquareIndex[perspective][pc] + KingBuckets[perspective][ksq]);
  }

  inline IndexType HalfKAv2_hm::finalize_index(Square s, const std::pair<IndexType, IndexType>& pIdx) {
    return IndexType((int(s) ^ pIdx.first) + pIdx.second);
  }

  // Get a list of indices for active features
  void HalfKAv2_hm::append_active_indices(
    const Position& pos,
    Color perspective,
    IndexList& active
  ) {
    Square ksq = pos.square<KING>(perspective);
    Bitboard bb = pos.pieces();
    while (bb)
    {
      Square s = pop_lsb(bb);
      active.push_back(make_index(perspective, s, pos.piece_on(s), ksq));
    }
  }


  // append_changed_indices() : get a list of indices for recently changed features

  void HalfKAv2_hm::append_changed_indices(
    Square ksq,
    const DirtyPiece& dp,
    Color perspective,
    IndexList& removed,
    IndexList& added
  ) {
    for (int i = 0; i < dp.dirty_num; ++i) {
      assert(dp.from[i] != SQ_NONE || dp.to[i] != SQ_NONE);
      std::pair<IndexType, IndexType> pi = make_index_parts(perspective, dp.piece[i], ksq);

      if (dp.from[i] != SQ_NONE)
        removed.push_back(finalize_index(dp.from[i], pi));

      if (dp.to[i] != SQ_NONE)
        added.push_back(finalize_index(dp.to[i], pi));
    }
  }

  int HalfKAv2_hm::update_cost(const StateInfo* st) {
    return st->dirtyPiece.dirty_num;
  }

  int HalfKAv2_hm::refresh_cost(const Position& pos) {
    return pos.count<ALL_PIECES>();
  }

  bool HalfKAv2_hm::requires_refresh(const StateInfo* st, Color perspective) {
    return st->dirtyPiece.piece[0] == make_piece(perspective, KING);
  }

}  // namespace Stockfish::Eval::NNUE::Features
