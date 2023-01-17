/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2023 The Stockfish developers (see AUTHORS file)

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
#include "../nnue_feature_transformer.h"

namespace Stockfish::Eval::NNUE::Features {

  // Index of a feature for a given king position and another piece on some square
  template<Color Perspective>
  inline IndexType HalfKAv2_hm::make_index(Square s, Piece pc, Square ksq) {
    return IndexType((int(s) ^ OrientTBL[Perspective][ksq]) + PieceSquareIndex[Perspective][pc] + KingBuckets[Perspective][ksq]);
  }
  
  void IndexTable::init(FeatureTransformer& ft) {

    std::memset(idxE, ~0U, sizeof(idxE));

    int idxCnt = 0;

    for (Color c : { WHITE, BLACK })
    {
        for (Square ks = SQ_A1; ks < SQUARE_NB; ++ks)
            for (Piece pc : { W_PAWN, W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
                              B_PAWN, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING })
                for (Square from = SQ_A1; from < SQUARE_NB; ++from)
                {
                    int idxFrom = c == WHITE ? HalfKAv2_hm::make_index<WHITE>(from, pc, ks)
                                             : HalfKAv2_hm::make_index<BLACK>(from, pc, ks);
                    idx[c][ks][pc][from] = idxFrom;

                    if (type_of(pc) == PAWN && (rank_of(from) == RANK_1 || rank_of(from) == RANK_8))
                        continue;

                    Bitboard bb = 0;

                    switch (pc)
                    {
                    case W_PAWN:
                    {
                        bb = pawn_attacks_bb(WHITE, from) | (from + NORTH);
                        if (rank_of(from) == RANK_2)                        
                            bb |= from + 2 * NORTH;
                        break;
                    }
                    case B_PAWN:
                    {
                        bb = pawn_attacks_bb(BLACK, from) | (from + SOUTH);
                        if (rank_of(from) == RANK_7)
                            bb |= from + 2 * SOUTH;
                        break;
                    }
                    case W_KNIGHT:
                    case B_KNIGHT:
                        bb = attacks_bb<KNIGHT>(from);
                        break;
                    case W_BISHOP:
                    case B_BISHOP:
                        bb = attacks_bb<BISHOP>(from);
                        break;
                    case W_ROOK:
                    case B_ROOK:
                        bb = attacks_bb<ROOK>(from);
                        break;
                    case W_QUEEN:
                    case B_QUEEN:
                        bb = attacks_bb<QUEEN>(from);
                        break;
                    case W_KING:
                    {
                        bb = attacks_bb<KING>(from);
                        if (from >= SQ_B1 && from <= SQ_G1) // Castling including FRC
                            bb |= SQ_G1 | SQ_C1;
                        break;
                    }
                    case B_KING:
                    {
                        bb = attacks_bb<KING>(from);
                        if (from >= SQ_B8 && from <= SQ_G8) // Castling including FRC
                            bb |= SQ_G8 | SQ_C8;
                        break;
                    }
                    default:
                        assert(0);
                    }

                    while (bb)
                    {
                        Square to = pop_lsb(bb);

                        if (idxE[c][ks][pc][to][from] != ~0U)
                            continue;

                        int idxTo = c == WHITE ? HalfKAv2_hm::make_index<WHITE>(to, pc, ks)
                                               : HalfKAv2_hm::make_index<BLACK>(to, pc, ks);

                        // Build composite vectors
                        WeightType* w = &ft.weights2[idxCnt * FeatureTransformer::HalfDimensions];
                        const WeightType* wFrom = &ft.weights[idxFrom * FeatureTransformer::HalfDimensions];
                        const WeightType*   wTo = &ft.weights[idxTo * FeatureTransformer::HalfDimensions];
                        for (IndexType i = 0; i < FeatureTransformer::HalfDimensions; ++i)
                            w[i] = wTo[i] - wFrom[i];

                        PSQTWeightType* pw = &ft.psqtWeights2[idxCnt * PSQTBuckets];
                        const PSQTWeightType* pwFrom = &ft.psqtWeights[idxFrom * PSQTBuckets];
                        const PSQTWeightType*   pwTo = &ft.psqtWeights[idxTo * PSQTBuckets];
                        for (IndexType i = 0; i < PSQTBuckets; ++i)
                            pw[i] = pwTo[i] - pwFrom[i];

                        idxE[c][ks][pc][from][to] = idxCnt++;

                        if (idxE[c][ks][pc][to][from] == ~0U)
                            idxE[c][ks][pc][to][from] = idxE[c][ks][pc][from][to] | (1U << 31);
                    }
                }

    }
    
    assert(idxCnt == 507392);
  };

  IndexTable indexTable;

  // Get a list of indices for active features
  template<Color Perspective>
  void HalfKAv2_hm::append_active_indices(
    const Position& pos,
    IndexList& active
  ) {
    Square ksq = pos.square<KING>(Perspective);
    Bitboard bb = pos.pieces();
    while (bb)
    {
      Square s = pop_lsb(bb);
      active.push_back(make_index<Perspective>(s, pos.piece_on(s), ksq));
    }
  }

  // Explicit template instantiations
  template void HalfKAv2_hm::append_active_indices<WHITE>(const Position& pos, IndexList& active);
  template void HalfKAv2_hm::append_active_indices<BLACK>(const Position& pos, IndexList& active);

  // append_changed_indices() : get a list of indices for recently changed features
  template<Color Perspective>
  void HalfKAv2_hm::append_changed_indices(
    Square ksq,
    const DirtyPiece& dp,
    IndexList& removed,
    IndexList& added,
    IndexList& changed
  ) {

    const uint32_t(&idx3D)[PIECE_NB][SQUARE_NB][SQUARE_NB] = indexTable.idxE[Perspective][ksq];

    for (int i = 0; i < dp.dirty_num; ++i) {
      
      if (dp.from[i] != SQ_NONE)
      {
        if (dp.to[i] != SQ_NONE)
        {
          assert((idx3D[dp.piece[i]][dp.from[i]][dp.to[i]] & ~(1U << 31)) < 507392);
          changed.push_back(idx3D[dp.piece[i]][dp.from[i]][dp.to[i]]);
        }
        else
          removed.push_back(make_index<Perspective>(dp.from[i], dp.piece[i], ksq));

        continue;
      }
      if (dp.to[i] != SQ_NONE)
        added.push_back(make_index<Perspective>(dp.to[i], dp.piece[i], ksq));
    }
  }

  // Explicit template instantiations
  template void HalfKAv2_hm::append_changed_indices<WHITE>(Square ksq, const DirtyPiece& dp, IndexList& removed, IndexList& added, IndexList& changed);
  template void HalfKAv2_hm::append_changed_indices<BLACK>(Square ksq, const DirtyPiece& dp, IndexList& removed, IndexList& added, IndexList& changed);

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
