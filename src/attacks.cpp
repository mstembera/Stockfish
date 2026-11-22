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

#include "attacks.h"

#include <array>

#include "misc.h"

namespace Stockfish::Attacks {

Bitboard LineBB[SQUARE_NB][SQUARE_NB];
Bitboard BetweenBB[SQUARE_NB][SQUARE_NB];
Bitboard inverse_RayPassBB[SQUARE_NB][SQUARE_NB];
Bitboard DiagBB[SQUARE_NB];
Bitboard AntiDiagBB[SQUARE_NB];

static constexpr Bitboard line_mask(Square sq, Direction d1, Direction d2) {
    Bitboard mask = 0;
    for (Direction d : {d1, d2})
    {
        Square s = sq;
        while (Bitboard dest = safe_destination(s, d))
        {
            mask |= dest;
            s += d;
        }
    }
    return mask;
}

extern constexpr DualMagicTable DualMagics = [] {
    DualMagicTable table{};
    for (Square s = SQ_A1; s <= SQ_H8; ++s)
        table.masks[s] = {line_mask(s, NORTH_EAST, SOUTH_WEST),
                          line_mask(s, NORTH_WEST, SOUTH_EAST), file_bb(s) ^ s, square_bb(s)};

    for (int occ = 0; occ < 128; occ += 2)
        for (int file = 0; file < FILE_NB; ++file)
            table.rankAttacks[occ * 4 + file] = u8(sliding_attack(ROOK, Square(file), occ));
    return table;
}();

void init() {

    for (Square s1 = SQ_A1; s1 <= SQ_H8; ++s1)
    {
        for (PieceType pt : {BISHOP, ROOK})
            for (Square s2 = SQ_A1; s2 <= SQ_H8; ++s2)
            {
                if (PseudoAttacks[pt][s1] & s2)
                {
                    LineBB[s1][s2] = (attacks_bb(pt, s1, 0) & attacks_bb(pt, s2, 0)) | s1 | s2;
                    BetweenBB[s1][s2] =
                      (attacks_bb(pt, s1, square_bb(s2)) & attacks_bb(pt, s2, square_bb(s1)));
                    inverse_RayPassBB[s2][s1] =
                      attacks_bb(pt, s1, 0) & (attacks_bb(pt, s2, square_bb(s1)) | s2);
                }
                BetweenBB[s1][s2] |= s2;
            }

        DiagBB[s1]     = line_mask(s1, NORTH_EAST, SOUTH_WEST) | s1;
        AntiDiagBB[s1] = line_mask(s1, NORTH_WEST, SOUTH_EAST) | s1;
    }
}


}  // namespace Stockfish::Attacks
