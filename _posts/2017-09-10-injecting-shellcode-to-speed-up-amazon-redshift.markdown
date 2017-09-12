---
layout: post
author: Michael Burge
title: "Injecting a Chess Engine into Amazon Redshift"
started_date: 2017-09-04 12:05:00 -0700
date: 2017-09-10 13:00:00 -0700
tags:
  - c
  - assembly
  - redshift
  - sql
js_files:
  - /assets/articles/20170910-redshift/jquery-3.2.1.min.js
  - /assets/articles/20170910-redshift/chessboardjs-themes.js
  - /assets/articles/20170910-redshift/chessboard-0.3.0.js
  - /assets/articles/20170910-redshift/demo-game.js
css_files:
  - /assets/articles/20170910-redshift/article.css
  - /assets/articles/20170910-redshift/chessboard-0.3.0.css
---

Amazon Redshift is a SQL-based database with a frontend similar to PostgreSQL. The backend compiles queries to C++ code, which execute against a columnnar datastore. When I last benchmarked it for a former employer, well-tuned queries were about 3-5x slower than hand-rolled C++. But I also uncovered an interesting technique that lets you close that gap for CPU-bound queries.

This article will cover:
* Writing a chess engine in C
* Turning that C into plain x86-64 instructions
* Executing those instructions on a running Amazon Redshift database

## The Result

Here's the first game I successfully played against the engine, on a real Redshift database:
<div id="demo-game" class="chessboard">
</div>
<div class="chessboard-controls">
  <input type="button" id="previousBtn" value="<<" />
  <input type="button" id="nextBtn" value=">>" />
</div>

If you'd like to try it on your live production database, you can simply run the files `create-apply-move.sql` and `create-best-move.sql` in the [Github repository](https://github.com/MichaelBurge/redshift-shellcode/tree/master/sql). These will create two functions `pf_apply_move` and `pf_best_move`.

Both take as input a FEN string representing the board. The initial state of the board is `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.

* `pf_best_move` takes a FEN string and a search depth, and produces a move with the highest expected score:
{% highlight sql %}
# select pf_best_move('4kb1r/p1p2p2/5n1p/2qp2p1/3rp1b1/2P3P1/PPQPBP1P/RNB2KNR w k - 0 1', 3);
 pf_best_move 
--------------
 e2a6
(1 row)
{% endhighlight %}

* `pf_apply_move` will produce a FEN string representing the new board after a move:
{% highlight sql %}
dev=# select pf_apply_move('4kb1r/p1p2p2/5n1p/2qp2p1/3rp1b1/2P3P1/PPQPBP1P/RNB2KNR w k - 0 1', 'e2a6');                                                                                  
                           pf_apply_move                           
-------------------------------------------------------------------
 4kb1r/p1p2p2/B4n1p/2qp2p1/3rp1b1/2P3P1/PPQP1P1P/RNB2KNR b k - 0 1
(1 row)
{% endhighlight %}

Most moves are 4 characters, and represent the file and rank for the source and destination location. You can specify which promotion a pawn gets by adding `/R, /N, /Q, /B`, so that `a7a8/Q` is a move that moves the pawn on `a7` to `a8` promoting it to a Queen.

Invalid input usually results in an exception being thrown.

### The Simplest Example

Let's take the following C as an example:
{% highlight c %}
int x() { return 42; }
{% endhighlight %}

You can turn this into assembly instructions with gcc:
{% highlight bash %}
$ gcc -c -O3 simple.c
$ objdump simple.o -d

simple.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <x>:
   0:   b8 2a 00 00 00          mov    $0x2a,%eax
   5:   c3                      retq   
{% endhighlight %}

So our shellcode is `0xb82a000000c3`. Here's a Python script that you can run on your own computer to execute it:
{% highlight python %}
from ctypes import *

shellcode = b'\xb8\x2a\x00\x00\x00\xc3'

libc = CDLL('libc.so.6')

src_ptr = c_char_p(shellcode)
size = len(shellcode)

code_ptr = libc.valloc(size)
code_ptr = c_void_p(code_ptr)

memmove(code_ptr, src_ptr, size)
err = libc.mprotect(code_ptr, size, 0x7)
if 0 != err:
    raise Exception("mprotect: " + str(code))

fptr = cast(code_ptr, CFUNCTYPE(c_long))
print fptr()
libc.free(code_ptr)
{% endhighlight %}

When I found it, Amazon had recently launched Python User-Defined Functions(UDFs). The Python translates pretty directly to this SQL:
{% highlight sql %}
create or replace function pf_simple () returns int stable as $$
from ctypes import (CDLL, c_long, c_char_p, c_void_p, memmove, cast, CFUNCTYPE)
def execute():
    shellcode = b'\xb8\x2a\x00\x00\x00\xc3'

    libc = CDLL('libc.so.6')

    src_ptr = c_char_p(shellcode)
    size = len(shellcode)

    code_ptr = libc.valloc(size)
    code_ptr = c_void_p(code_ptr)

    memmove(code_ptr, src_ptr, size)
    err = libc.mprotect(code_ptr, size, 0x7)
    if 0 != err:
        raise Exception("mprotect: " + str(err))

    fptr = cast(code_ptr, CFUNCTYPE(c_long))
    result = fptr()
    libc.free(code_ptr)
    return result
return execute()
$$ language plpythonu;
{% endhighlight %}

which works exactly as expected:
{% highlight sql %}
dev=# select pf_simple();
 pf_simple 
-----------
        42
(1 row)
{% endhighlight %}

When is it appropriate to use this? Practically speaking these UDFs are really slow to call(10,000 calls/second), so it was actually pretty hard to find a case to use them. But if you have a small number of CPU-intensive calls, then the overhead doesn't matter as much and the benefit of C is larger.

Like a chess engine.

## Chess Engine

A chess engine consists of several components:
* Board representation
* Move generation
* Board evaluation
* Move search

We're going to write our own chess engine in C, because we have some very restrictive deployment requirements and C gives us the best balance of control, readability, and ease of debugging. If we used a language like Go, Haskell, or Python, we would have to deploy not only our code but also the implementation of these languages. Our C will map directly to reasonable assembly, and carries no additional implementation that we would need to debug.

### Board Representation

There are 64 squares on a chess board, and modern Intel CPUs are 64-bit. So we'll use __bitsets__ to represent where each piece is on the board:
{% highlight cpp %}
// bb is 'bitboard'
typedef struct gamestate {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t kings_bb;
  uint64_t pawns_bb;
  uint64_t current_player_bb;
  uint64_t castle_flags;
  int en_passant_sq;
  bool is_white;
} gamestate;

// CASTLE FLAGS
const int CASTLE_WHITE_KINGSIDE = 0x1;
const int CASTLE_WHITE_QUEENSIDE = 0x2;
const int CASTLE_BLACK_KINGSIDE = 0x4;
const int CASTLE_BLACK_QUEENSIDE = 0x8;
{% endhighlight %}

Each __bitboard__ consists of 64 bits, which are set to 1 if the corresponding square contains that piece. `current_player_bb` is set to 1 if White controls the piece on that square.

`castle_flags` are 4 bits: Whether white or black can castle kingside or queenside.

Pawns are allowed to move 2 spaces on their first move. If player A uses this to avoid an enemy pawn's capture, the __En Passant__ rule allows player B to capture it regardless. The `en_passant_sq` is the coordinate of the skipped square from the previous move, or a special "invalid coordinate" value if no double-move occurred.

It will be convenient to develop the engine by assuming that White is always the player ready to move. This lets us hardcode the positions for castling, en passant, and pawn double-movement. We'll flip the board after every move, and use the `is_white` property to track the real color of the current player.

Bitsets are easy to efficiently combine:
{% highlight cpp %}
uint64_t all_pieces(gamestate x)
{
  return
    x.rooks_bb |
    x.knights_bb |
    x.bishops_bb |
    x.queens_bb |
    x.kings_bb |
    x.pawns_bb
    ;
}

uint64_t enemy_pieces(gamestate x)
{
  return all_pieces(x) ^ x.current_player_bb;
}
{% endhighlight %}

We also need to know how to map an index from 0-63 to a bitset's bit. Here's how to do that:
{% highlight cpp %}
uint64_t mkPosition(int file, int rank)
{
  return rank * RANK + file;
}
rank(int x) { return x / 8; }
file(int x) { return x % 8; }

const int POSITION_INVALID = 255;
{% endhighlight %}

The decision to put the file coordinate first was intentional. Since each file is a contiguous byte, changing the endianness of a 64-bit bitset will change the order of the rows without changing their contents:
{% highlight cpp %}
uint64_t flip_bb(uint64_t x)
{
  return __builtin_bswap64(x);
}
// Flips the board so white is black and black is white.
// In our model, it is always white's turn to move.
gamestate swap_board(gamestate g)
{
  g.current_player_bb ^= all_pieces(g);
  g.rooks_bb   = flip_bb(g.rooks_bb);
  g.knights_bb = flip_bb(g.knights_bb);
  g.bishops_bb = flip_bb(g.bishops_bb);
  g.queens_bb  = flip_bb(g.queens_bb);
  g.kings_bb   = flip_bb(g.kings_bb);
  g.pawns_bb   = flip_bb(g.pawns_bb);
  g.current_player_bb = flip_bb(g.current_player_bb);
  if (g.en_passant_sq != POSITION_INVALID) {
    g.en_passant_sq = mkPosition(file(g.en_passant_sq), 7 - rank(g.en_passant_sq));
  }
  {
    uint64_t flags = 0;
    if (g.castle_flags & CASTLE_WHITE_KINGSIDE)
      flags |= CASTLE_BLACK_KINGSIDE;
    if (g.castle_flags & CASTLE_WHITE_QUEENSIDE)
      flags |= CASTLE_BLACK_QUEENSIDE;
    if (g.castle_flags & CASTLE_BLACK_KINGSIDE)
      flags |= CASTLE_WHITE_KINGSIDE;
    if (g.castle_flags & CASTLE_BLACK_QUEENSIDE)
      flags |= CASTLE_WHITE_QUEENSIDE;
    g.castle_flags = flags;
  }
  return g;
}
{% endhighlight %}

Here's two examples of how to construct one of these `gamestate` values:
{% highlight cpp %}
// An empty board
private gamestate zerostate()
{
  gamestate x;
  x.current_piece_bb = 0;
  x.rooks_bb = 0;
  x.knights_bb = 0;
  x.bishops_bb = 0;
  x.queens_bb = 0;
  x.kings_bb = 0;
  x.pawns_bb = 0;
  x.en_passant_sq = POSITION_INVALID;
  x.castle_flags = 0;
  x.is_white = true;
  return x;
}
// A standard chess board at the beginning of a game
private gamestate new_game()
{
  gamestate x;
  x.rooks_bb =
    bit(mkPosition(0,0)) |
    bit(mkPosition(7,0)) |
    bit(mkPosition(0,7)) |
    bit(mkPosition(7,7));
  x.knights_bb =
    bit(mkPosition(1,0)) |
    bit(mkPosition(6,0)) |
    bit(mkPosition(1,7)) |
    bit(mkPosition(6,7));
  x.bishops_bb =
    bit(mkPosition(2,0)) |
    bit(mkPosition(5,0)) |
    bit(mkPosition(2,7)) |
    bit(mkPosition(5,7));
  x.queens_bb =
    bit(mkPosition(3,0)) |
    bit(mkPosition(3,7));
  x.kings_bb =
    bit(mkPosition(4,0)) |
    bit(mkPosition(4,7));
  x.pawns_bb =
    ((uint64_t)0xFF << RANK) |
    ((uint64_t)0xFF << (6*RANK));
  x.current_player_bb = 0xFFFF;
  x.en_passant_sq = POSITION_INVALID;
  x.castle_flags = 0xF;
  x.is_white = true;
  return x;
}

uint64_t bit(int idx);
{% endhighlight %}

Finally, we'll need some bit-twiddling helper functions:
{% highlight cpp %}
// bit(3) creates a single bit at the 3rd position: = 00000100
uint64_t bit(int idx) {
  if (idx >= 64 || idx < 0) {
    return 0;
  }
  return ((uint64_t)(1) << idx);
}
// clear_bit(bit(3), 3) = 0
uint64_t clear_bit(uint64_t x, uint64_t idx) { return x & ~bit(idx); }
bool is_bit_set(uint64_t x, uint64_t idx) {
  if (idx >= 64) { return false; }
  return x & bit(idx);
}
uint64_t set_bit(uint64_t x, uint64_t idx) { return x | bit(idx); }
// lsb_first_set returns the index of the first set bit, starting from the "least significant bit".
uint64_t lsb_first_set(uint64_t x) { return __builtin_ctzll(x); }
// msb = "most significant bit"
uint64_t msb_first_set(uint64_t x) { return (63 - __builtin_clzll(x)); }
{% endhighlight %}

## Move Generation

A common operation is to efficiently list all possible moves from a given position. With one exception, we'll say that a move is a pair of coordinates:
{% highlight cpp %}
typedef int position;

struct move {
  position from;
  position to;
};
gamestate apply_move(gamestate g, move m);
{% endhighlight %}

Once we've obtained a move, we can use `apply_move` to update our board, including removing captured pieces, updating whether the king can still castle, and swapping the board.

The "one exception" is that a pawn can promote to either a rook, knight, bishop, or queen once it reaches the 8th rank. Since we only need 6 bits to store the 64 chessboard positions, I placed the promotion choice in the `to` field:
{% highlight cpp %}
typedef int piece;

const piece PIECE_EMPTY  = 0;
const piece PIECE_ROOK   = 1;
const piece PIECE_KNIGHT = 2;
const piece PIECE_BISHOP = 3;
const piece PIECE_QUEEN  = 4;
const piece PIECE_KING   = 5;
const piece PIECE_PAWN   = 6;

piece promotion_piece(move m)
{
  return (m.to >> 6);
}
position remove_promotion_bits(move m)
{
    return (m.to & 0x3F)
}
{% endhighlight %}

Notice that the promotable pieces start at 4 and count down to 0. This simplifies the move generation code later.

### Iterators

You might imagine that we would keep a list or array of these `move` values. That would require dynamic memory allocation, which would complicate the deployment of our chess engine. It will be simpler to keep a small data structure on the stack that we can use to generate the moves on-demand:
{% highlight cpp %}
typedef struct iterator {
  ...
} iterator;

iterator mkIterator(gamestate g);
iterator advance_iterator(gamestate g, iterator i);
bool is_iterator_finished(iterator x);
move dereference_iterator(iterator i);
{% endhighlight %}

If we had such a data structure, here's how we could use it to count the number of moves:
{% highlight cpp %}
private int num_available_moves(gamestate g)
{
  int count = 0;
  iterator i = mkIterator(g);
  while (! is_iterator_finished(i)) {
    // move m = dereference_iterator(i);
    count++;
    i = advance_iterator(g, i);
  }
  return count;
}
{% endhighlight %}

The bitboards make this pretty easy to implement. An iterator's job is to find the next available move, so it needs to:
* Find the piece type
* Find the specific piece on the board
* Find a move made by that piece
* If that piece is a pawn moving to promote, find a promotion piece

Here's a direct translation of that:
{% highlight cpp %}
typedef struct iterator {
  int piece_type;           // Counts from PIECE_PAWN to 0
  uint64_t specific_piece;  // Bitset containing all pieces of that type
  uint64_t available_moves; // Current piece's available moves
  int promotion_piece;      // Current target's promotion piece
} iterator;
{% endhighlight %}

The location of the specific piece is the first bit that's set within the `specific_piece` bitboard

The `gamestate` has bitboards for every piece type, so if we clear bits when we're done we can calculate `piece_type` from the first nonzero bitboard.

Here I've unrolled `piece_type` to make it look more like the `gamestate` type:
{% highlight cpp %}
typedef struct iterator {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t kings_bb;
  uint64_t pawns_bb;
  uint64_t current_piece_moves_bb;
  uint64_t promotion_piece;  
} iterator;
{% endhighlight %}

Since we already have a `gamestate` when we create the `iterator`, I actually reused the data structure to be both. It's probably a better idea to use the 4-element type above, but here's the real `iterator` type from my engine:

{% highlight cpp %}
typedef struct gamestate {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t kings_bb;
  uint64_t pawns_bb;
  union {
    uint64_t current_player_bb;
    uint64_t current_piece_bb; // For iterators
  };
  int en_passant_sq;
  union {
    uint64_t castle_flags;
    uint64_t promotion_piece; // For iterators
  };
  bool is_white;
} gamestate;

typedef gamestate iterator;
{% endhighlight %}

Most of the fields on a gamestate only need to be restricted to the current player to be directly usable:
{% highlight cpp %}
iterator mkIterator(gamestate g)
{
  iterator x = g;
  x.rooks_bb   &= x.current_player_bb;
  x.knights_bb &= x.current_player_bb;
  x.bishops_bb &= x.current_player_bb;
  x.queens_bb  &= x.current_player_bb;
  x.kings_bb   &= x.current_player_bb;
  x.pawns_bb   &= x.current_player_bb;
  x.promotion_piece = 0;
  
  x = reset_iterator_moves(g, x);
  if (! x.current_piece_bb) {
    x = advance_iterator(g, x);
  }
  
  return x;
}

iterator reset_iterator_moves(gamestate g, iterator i);

{% endhighlight %}

`reset_iterator_moves` sets the `current_piece_bb` bitset to have all of the available moves for the current piece. If the piece has no available moves, then `advance_iterator` has the logic for finding the next available piece and its moves.

{% highlight cpp %}

iterator reset_iterator_moves(gamestate g, iterator i)
{
  if (is_iterator_finished(i)) {
    return zerostate();
  } else {
    int idx = iterator_position(i);
    uint64_t moves = valid_piece_moves(g, idx);
    i.current_piece_bb = moves;
    i = reset_iterator_promotion_piece(g, i);
    return i;
  }
}

// The location of the piece we are emitting moves for
int iterator_position(iterator x);
// Calculates a bitset for the moves for a piece
uint64_t valid_piece_moves(gamestate x, int idx);
// Resets promotion information for a piece
iterator reset_iterator_promotion_piece(gamestate g, iterator i);
{% endhighlight %}

At this point, it's mostly simple case analysis to figure out the information from our `gamestate`. I'll show one last example below before moving on.
{% highlight cpp %}
uint64_t valid_piece_moves(gamestate x, int idx)
{
  int piece = get_piece(x, idx);
  switch (piece) {
  case PIECE_ROOK:
    return valid_rook_moves(x, idx);
  case PIECE_KNIGHT:
    return valid_knight_moves(x, idx);
  case PIECE_BISHOP:
    return valid_bishop_moves(x, idx);
  case PIECE_QUEEN:
    return valid_queen_moves(x, idx);
  case PIECE_KING:
    return valid_king_moves(x, idx);
  case PIECE_PAWN:
    return valid_pawn_moves(x, idx);
  default:
    abort();
  }
}

#define abort() __builtin_trap();
{% endhighlight %}

And here's how to implement one of those:
{% highlight cpp %}
const int DIRECTION_EAST  = 0;
const int DIRECTION_WEST  = 1;
const int DIRECTION_NORTH = 2;
const int DIRECTION_SOUTH = 3;

private uint64_t valid_rook_moves(gamestate x, int idx)
{
  return
    shoot_ray_until_blocker(x, idx, DIRECTION_NORTH) |
    shoot_ray_until_blocker(x, idx, DIRECTION_WEST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_EAST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_SOUTH)
    ;
}

uint64_t shoot_ray_until_blocker(gamestate state, int idx, int direction)
{
  uint64_t pieces = all_pieces(state);
  uint64_t base_ray = mkRay(idx, direction);
  uint64_t blockers = base_ray & pieces;
  int blocker = closest_blocker(blockers, direction);
  if (blocker == POSITION_INVALID) {
    return base_ray;
  } else {
    uint64_t blocker_ray = mkRay(blocker, direction);
    uint64_t movable_squares_without_capture = base_ray ^ blocker_ray;
    bool allow_capture = ! is_bit_set(state.current_player_bb, blocker);
    if (allow_capture)
      return movable_squares_without_capture | bit(blocker);
    else
      return movable_squares_without_capture & ~bit(blocker);
  }
}

int closest_blocker(uint64_t blockers_ray, int direction);
uint64_t mkRay(int center, int direction);
{% endhighlight %}

I've omitted the below from this article for brevity. You can find the full implementation of these on [Github](https://github.com/MichaelBurge/redshift-shellcode).
{% highlight cpp %}
bool is_iterator_finished(iterator x);
move dereference_iterator(iterator i);
int iterator_position(iterator x);
int get_piece(gamestate x, int idx);
iterator advance_iterator(gamestate g, iterator i);
iterator reset_iterator_promotion_piece(gamestate g, iterator i);
uint64_t valid_knight_moves(gamestate x, int idx);
uint64_t valid_bishop_moves(gamestate x, int idx);
uint64_t valid_queen_moves(gamestate x, int idx);
uint64_t valid_king_moves(gamestate g, int idx);
uint64_t valid_pawn_moves(gamestate x, int center);
{% endhighlight %}

The `iterator` generates "pseudo-legal" moves, with each call to `advance_iterator` being `O(1)`. Pseudo-legal moves are moves that could be legal, but require expensive additional information to make a final decision.

For example, a king cannot move into check. Check detection requires knowing whether the opponent has a pseudo-legal move that attacks the king. My naive implementation applies the move and iterates over all of the opponent's moves to see if any attack the king. Castling has a check detection on as many as 4 squares, and draws also have some restrictions.

Besides the efficiency, psuedo-legal moves can't be merged with legal moves because the rules depend on it. Consider the scenario below:
<div id="illegal" class="chessboard">
</div>

* Black has pinned White's bishop to his king.
* It is not legal for the white bishop to move to d5, because it leaves his king in check.
* When testing legality, an engine might apply the move anyways to see if it results in check.
* After moving to d5, it would not be legal for the black bishop to capture the white king, since it leaves his own king in check.
* However, it is pseudo-legal for the black bishop to capture the king, which is what the check calculation uses.

An engine that only had the concept of legal moves would thus contain a bug allowing the white king to move into check.

### Move Search

Besides generating a list of moves, our engine also needs to choose the best one. Suppose we had a function `true_score` that gave us a perfectly accurate rating of the board, in the sense that `true_score(g1) > true_score(g2)` implies that `g1` is more favorable to White than `g2`. Then we could calculate the perfect move as follows:

{% highlight cpp %}
const int NEGATIVE_INFINITY = 0x80000000;

move best_move(gamestate g, int depth)
{
  int max = NEGATIVE_INFINITY;
  move ret; ret.from = POSITION_INVALID; ret.to = POSITION_INVALID;
  iterator i = mkLegalIterator(g);
  while (! is_iterator_finished(i)) {
    move m = dereference_iterator(i);
    gamestate g_new = apply_move(g, m);
    int score = -true_score(g_new);
    if (score > max) {
      max = score;
      ret = m;
    }
    i = advance_iterator_legal(g, i);
  }
  return ret;
}

int true_score(gamestate g);

/* mkLegalIterator and advance_iterator_legal are variations on
 * mkIterator and advance_iterator that skip illegal moves.
 */
iterator mkLegalIterator(gamestate g);
iterator advance_iterator_legal(gamestate g, iterator i);
{% endhighlight %}

The minus sign on `true_score` is because it is scoring the opponent's turn.

So the problem reduces to finding a good "score" for a board position. Here's a perfect one:

{% highlight cpp %}
// Includes draws too
const int VALUE_CHECKMATE = -1;

int true_score(gamestate g)
{
  iterator i = mkLegalIterator(g);
  if (is_iterator_finished(i)) {
    int color = g.is_white ? 1 : -1;
    return color * VALUE_CHECKMATE;
  }
  int max = NEGATIVE_INFINITY;
  while (! is_iterator_finished(i)) {
    move m = dereference(iterator(i));
    gamestate g_new = apply_move(g, m);
    int score = -true_score(g_new);
    if (score > max) {
      max = score;
    }

    i = advance_iterator_legal(g, i);
  }
  return ret;
}
{% endhighlight %}

The 50-move rule makes it so that every chess game eventually terminates, so the recursion will always eventually terminate. If you count draws as losses, [every finite game has a winning strategy for one of the players](https://en.wikipedia.org/wiki/Minimax_theorem).

The idea behind `true_score` is that your last move is either the last move in the game or the one before that. If you make the last move, it means you lose or draw, so you get a -1 score. Otherwise, you've won or drawn, and since we're counting the player that draws as having lost, you must've won. Since `true_score(g1) = 1 > true_score(g2) = -1` implies that White wins in `g1` and loses in `g2`, `true_score` is perfectly accurate when comparing leaf nodes or 2nd-to-leaf nodes. Recursion handles the rest.

It's not computationally feasible to calculate `true_score`. However, rather than recurse all the way out to checkmate, we could stop after N moves and guess whether we'll be checkmated. The guess could take the form of a negative number, with lower numbers meaning we're more likely to be checkmated. 
{% highlight cpp %}
const int VALUE_NEGAMAX_START = 0x80000000;
private int negamax(gamestate g, int depth, int color)
{
  if (depth == 0)
    return color*evaluate(g);
  int max = VALUE_NEGAMAX_START;
  for (iterator i = mkLegalIterator(g); ! is_iterator_finished(i); i = advance_iterator_legal(g, i))  {
    move m = dereference_iterator(i);
    gamestate g2 = apply_move(g, m);
    int score = -negamax(g2, depth-1, color*-1);
    if (score > max)
      max = score;
  }
  return max;
}
int evaluate(gamestate g);
{% endhighlight %}

Since we only ever take the `maximum` of two scores, the actual score values that `evaluate` returns don't matter at all and only establish an ordering.

### Evaluation

The personality of a chess engine is in the evaluation function. You can make it play more aggressively by raising the score for being able to attack enemy units, or for having pieces in the center.

Here are the 3 basic scoring functions we'll be using:

{% highlight cpp %}
int evaluate(gamestate g)
{
  return score_pieces(g) + score_availability(g) + score_attacking(g);
}

int score_center(gamestate g)
{
  int num_centers = num_bits(g.current_player_bb & center());
  return num_centers * VALUE_CENTER;
}

int score_attacking(gamestate g)
{
  return num_bits(enemy_pieces(g) & movepoints(g));
}

int score_availability(gamestate g)
{
  int num_moves = num_legal_moves(g);
  if (num_moves == 0)
    return VALUE_CHECKMATE;
  return VALUE_AVAILABLE_MOVE * num_moves;
}

uint64_t center()
{
  return
    bit(mkPosition(2,2)) |
    bit(mkPosition(2,3)) |
    bit(mkPosition(2,4)) |
    bit(mkPosition(2,5)) |
    
    bit(mkPosition(3,2)) |
    bit(mkPosition(3,3)) |
    bit(mkPosition(3,4)) |
    bit(mkPosition(3,5)) |
    
    bit(mkPosition(4,2)) |
    bit(mkPosition(4,3)) |
    bit(mkPosition(4,4)) |
    bit(mkPosition(4,5)) |
    
    bit(mkPosition(5,2)) |
    bit(mkPosition(5,3)) |
    bit(mkPosition(5,4)) |
    bit(mkPosition(5,5));    
}

// A bitset for all possible move destinations.
uint64_t movepoints(gamestate g);
int num_legal_moves(gamestate g);
int num_bits(uint64_t x);
{% endhighlight %}

Notice that we've moved `true_score`s checkmate score into `score_availability`.

### Testing

Before we do anything else, we need to be certain we don't have obvious bugs in our engine, like generating invalid moves, crashes, missing moves, or having a sign error in our search algorithm. And we can't easily debug this code once it's on Redshift.

The two big components are our move generation, and our search algorithm. The search algorithm is tested by unit tests with a few clear-cut initial positions that have one obviously-good move. The move generation is more interesting.

The gold standard for testing move generation is to count the number of states N moves deep from several initial positions, and compare it with known [reference values](https://chessprogramming.wikispaces.com/Perft+Results). Here are the first few:

| depth |   leaf nodes |
|-------|--------------|
|0      |      1       |
|1      |      20      |
|2      |     400      |
|3      |    8,902     |
|4      |  197,281     |

The first entry is `1` because there is exactly one possible board when you start a game of chess and don't do anything to it.

The second entry is `20` because there are 20 possible moves from the starting position: 4 knight moves + 8 pawn single-jumps + 8 pawn double-jumps.

The third entry is `400 = 20*20` because black has the same 20 moves as white. Since there are two independent choices, they multiply.

If it works, you can have great confidence in the correctness of your move generator. But when it inevitably turns up errors, all you get is that an error exists: "Expected 197,281 moves; found 198,303".

To debug a __perft__ test failure, you implement __divide__. `divide(n)` calculates `perft(n-1)` for each possible move. If you compare the output of `divide` with another chess engine and find a difference on some move, then checking `perft(n-1)` has reduced the size of your search space. You can repeat `divide` until you find a move that doesn't exist on one of the two engines.

I used [Roce](http://www.rocechess.ch/perft.html) when debugging my chess engine. I recommend writing a unit test whenever `perft` uncovers an error, since you'll inevitably want to "optimize" your move generation later and it's much easier to work with small constructive tests.

With this, we're essentially done with development for now. Now we'll take the result and deploy it onto Amazon Redshift.

## Deployment

How do we actually get this thing running on Redshift? First we need to get it running locally using a Python script. Then we'll deploy it to Redshift using a Python UDF.

### Code Generation

Recall the original example used to validate our approach:

{% highlight c %}
int x() { return 42; }
{% endhighlight %}

which we turned into shellcode by running gcc:
{% highlight bash %}
$ gcc -c -O3 simple.c
$ objdump simple.o -d

simple.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <x>:
   0:   b8 2a 00 00 00          mov    $0x2a,%eax
   5:   c3                      retq   
{% endhighlight %}

A couple things to note about this:
* The target `x` is the first symbol, so jumping to the shellcode pointer will begin execution there.
* There are no absolute addresses.
* There is only a single section
* It uses common instructions that our CPU supports.

Lets look at a few examples to see how these assumptions can fail:

### Using Multiple Symbols

Lets use a recursive function so that lower optimization levels emit a symbol for it:

{% highlight cpp %}
// gcc -c -O simple-2.c

int custom_main(int l);
static int factorial(int x);

// l = 5 implies 1 + 1 + 2 + 6 + 24 = 34
int custom_main(int l) {
  int ret = 0;
  for (int i = 0; i < l; i++) {
    ret += factorial(i);
  }
  return ret;
}

static int factorial(int x) {
  if (x == 0) { return 1; }
  return x * factorial(x-1);
}
{% endhighlight %}

Notice that `factorial` has been placed first in the disassembly:

{% highlight bash %}
$ objdump -d simple-2.o

simple-2.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <factorial>:
   0:   b8 01 00 00 00          mov    $0x1,%eax
   5:   85 ff                   test   %edi,%edi
   7:   74 0f                   je     18 <factorial+0x18>
   9:   53                      push   %rbx
   a:   89 fb                   mov    %edi,%ebx
   c:   8d 7f ff                lea    -0x1(%rdi),%edi
   f:   e8 ec ff ff ff          callq  0 <factorial>
  14:   0f af c3                imul   %ebx,%eax
  17:   5b                      pop    %rbx
  18:   f3 c3                   repz retq 

000000000000001a <custom_main>:
  1a:   41 54                   push   %r12
  1c:   55                      push   %rbp
  1d:   53                      push   %rbx
  1e:   85 ff                   test   %edi,%edi
  20:   7e 20                   jle    42 <main+0x28>
  22:   41 89 fc                mov    %edi,%r12d
  25:   bb 00 00 00 00          mov    $0x0,%ebx
  2a:   bd 00 00 00 00          mov    $0x0,%ebp
  2f:   89 df                   mov    %ebx,%edi
  31:   e8 ca ff ff ff          callq  0 <factorial>
  36:   01 c5                   add    %eax,%ebp
  38:   83 c3 01                add    $0x1,%ebx
  3b:   41 39 dc                cmp    %ebx,%r12d
  3e:   75 ef                   jne    2f <main+0x15>
  40:   eb 05                   jmp    47 <main+0x2d>
  42:   bd 00 00 00 00          mov    $0x0,%ebp
  47:   89 e8                   mov    %ebp,%eax
  49:   5b                      pop    %rbx
  4a:   5d                      pop    %rbp
  4b:   41 5c                   pop    %r12
  4d:   c3                      retq   
{% endhighlight %}

So if we jump to the beginning of our shellcode, we will end up in `factorial`. And because `factorial` is a `static` function, I don't believe it is required to be callable with the standard calling convention like `custom_main` is, so a function like it may even be unsafe to call.

Here's how to add an `offset` that we can use to jump to the correct function:

{% highlight python %}
from ctypes import *
shellcode = b'\xb8\x01\x00\x00\x00\x85\xff\x74\x0f\x53\x89\xfb\x8d\x7f\xff\xe8\xec\xff\xff\xff\x0f\xaf\xc3\x5b\xf3\xc3\x41\x54\x55\x53\x85\xff\x7e\x20\x41\x89\xfc\xbb\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x89\xdf\xe8\xca\xff\xff\xff\x01\xc5\x83\xc3\x01\x41\x39\xdc\x75\xef\xeb\x05\xbd\x00\x00\x00\x00\x89\xe8\x5b\x5d\x41\x5c\xc3'
# offset = 0
offset = 26 # Decimal conversion of 1a, the beginning of custom_main
libc = CDLL('libc.so.6')

def get_executable_buffer(shellcode):
    src_ptr = c_char_p(shellcode)
    size = len(shellcode)

    code_ptr = libc.valloc(size)
    code_ptr = c_void_p(code_ptr)

    memmove(code_ptr, src_ptr, size)
    err = libc.mprotect(code_ptr, size, 0x7)
    if 0 != err:
        raise Exception("mprotect: " + str(code))
    return code_ptr, size

def execute():
    code_ptr, size = get_executable_buffer(shellcode)

    # NOTE: Here we adjust the jump destination using the offset
    main_ptr = c_void_p(c_uint64(code_ptr.value).value + c_uint64(offset).value)
    SHELLCODE_T = CFUNCTYPE(c_int, c_int)
    
    fptr = cast(main_ptr, SHELLCODE_T)
    result = fptr(5)
    libc.free(code_ptr)
    return result

print execute()
{% endhighlight %}

* If `offset` is 0, it will invoke `factorial` and print `120`.
* If `offset` is 26, it will invoke `custom_main` and print 34, which is the sum of `0! + 1! + 2! + 3! + 4!`.

### Handling Absolute Addresses

You might think all assembly code either uses relative addresses, use hardware or OS-provided absolute addresses, or obtains absolute addresses at runtime from the OS. You might think this because the kernel cannot place programs at fixed locations in memory: If you run a self-modifying program twice, its address space would conflict and the code could not be shared.

Here's a C program to prove that notion wrong:
{% highlight cpp %}
#include <stdio.h>

extern void _start();

int main()
{
  printf("%lx\n", &_start);
  return 0;
}
{% endhighlight %}

On my machine, this prints `0x400430`. We can see the same number in the output of `readelf -l a.out`:
{% highlight bash %}
$ gcc absolute.c
$ readelf -l a.out

Elf file type is EXEC (Executable file)
Entry point 0x400430
There are 9 program headers, starting at offset 64
{% endhighlight %}

This by itself doesn't mean anything, because ELF headers have no meaning when we are directly jumping to bytes in memory. However, the example C code clearly takes the address of `_start`, and this absolute address is visible in the disassembly:
{% highlight bash %}
0000000000400526 <main>:
  400526:       55                      push   %rbp
  400527:       48 89 e5                mov    %rsp,%rbp
  40052a:       be 30 04 40 00          mov    $0x400430,%esi
  40052f:       bf d4 05 40 00          mov    $0x4005d4,%edi
  400534:       b8 00 00 00 00          mov    $0x0,%eax
  400539:       e8 c2 fe ff ff          callq  400400 <printf@plt>
  40053e:       b8 00 00 00 00          mov    $0x0,%eax
  400543:       5d                      pop    %rbp
  400544:       c3                      retq   
  400545:       66 2e 0f 1f 84 00 00    nopw   %cs:0x0(%rax,%rax,1)
  40054c:       00 00 00 
  40054f:       90                      nop
{% endhighlight %}

Notice the `mov $0x400430,%esi` line. The constant `0x400430` is "backwards" in the actual byte encoding to the left of it: `30 04 40 00`. It's backwards because Intel CPUs use a "little-endian" architecture where the bytes are arranged least-significant to most-significant, while the bits inside each byte are arranged most-significant to least-significant. People are taught in school to write numbers from most-significant to least-significant, so the individual bytes seem correct to us but their order seems backwards.

It's important to look at the actual bytes here because the disassembler will sometimes "lie" to you by converting relative addresses to absolute addresses. The `callq` instruction is not an example of an absolute address, because the bytes `c2 fe ff ff` are actually the signed integer representation of `-318`. The final executable locates `_printf@plt` exactly 318 bytes before this call instruction, `0x400539 - 0x400400 = 313`, and the remaining 5 bytes are the `callq` instruction itself.

So how does the kernel do this? If we ran two different programs that wanted to start at address `0x400430`, they can't both be physically placed there. However, Intel CPUs have a __Memory Management Unit__ that translates __virtual addresses__ into __physical addresses__. The kernel is one step up from our C code, but even it doesn't truly have that level of access: If you run your computer in a virtual machine, the guest OS is behind another layer of indirection with __Extended Page Tables__.

When the kernel loads this ELF file and grants it a PID, it uses the MMU to map `0x400430` to a real address that we don't have access to. The kernel-visible address space is mounted at `/dev/mem`, which only root can access. It's also available as a core dump in `/proc/kcore` so that you can use tools like `gdb` to debug the kernel.

Having any absolute addresses - even virtual ones - is a huge problem for us, because we haven't told the Linux kernel running Amazon Redshift to map our shellcode at address `0x400430`. We can resolve this in two ways:

* Map our shellcode at the correct virtual address
* Tell the compiler not to emit absolute addresses

And here's how to do these:
#### mmap

The `printf` adds a lot of noise, so we'll use a cut-down example:

{% highlight c %}
int y() { return 42; }
typedef int (*shellcode_t)();
long custom_main() { return ((shellcode_t)0x400000)(); }
{% endhighlight %}

Here, `custom_main` is our intended entry point. It calls the function at absolute address `0x400000` and passes the return value along.

My `python` happens to have its own entry point at `0x49d9b0`, which is somewhat close to the desired absolute address. If it overlapped `0x400000`, we would not easily be able to use Python for our loader. In such a case, we could recompile `python` with a different absolute address or use a loader written in C. See [mmap-test.c](https://github.com/MichaelBurge/redshift-shellcode/blob/master/mmap-test.c) in the Github repository for an example of a loader written in C.

Since there's no conflict, here's how to use `mmap` to assign our shellcode to a specific location in virtual memory:
{% highlight python %}
from ctypes import (CDLL, c_long, c_char_p, c_void_p, memmove, cast, CFUNCTYPE, c_uint64, Structure, c_int, c_char, create_string_buffer)

#from shellcode_bytes import shellcode, offset
shellcode = b'\x55\x48\x89\xe5\xb8\x2a\x00\x00\x00\x5d\xc3\x55\x48\x89\xe5\xb8\x00\x00\x00\x00\xba\x00\x00\x40\x00\xff\xd2\x48\x98\x5d\xc3';
offset = 11
#offset = 26
entry_address = 0x400000

MAP_FLAGS = 0x32

def execute():
    libc = CDLL('libc.so.6')

    src_ptr = c_char_p(shellcode)
    size = len(shellcode)
    
    code_ptr = libc.mmap(entry_address, size, 0x7, MAP_FLAGS, -1, 0)
    if (code_ptr == -1):
        libc.err(1, "mmap failed")
        return ""
    code_ptr = c_void_p(code_ptr)
    memmove(code_ptr, src_ptr, size)

    main_ptr = c_void_p(c_uint64(code_ptr.value).value + c_uint64(offset).value)
    SHELLCODE_T = CFUNCTYPE(c_long)
    
    fptr = cast(main_ptr, SHELLCODE_T)
    result = fptr()
    #fptr(fen, move, result_buf)
    # libc.free(code_ptr)
    return result

print(execute())
# Prints: 42
{% endhighlight %}

### Position-Independence

The other possibility is to simply use relative addresses for everything. I'll go through a couple sources of absolute addresses that I encountered and show how each can be suppressed:
* .text.startup
* Jump tables
* Data

#### Main symbol

`gcc` likes to place any function named `main` into its own ELF section. This breaks address continuity with other sections. Here's a simple example:

{% highlight cpp %}
// gcc -O2 -c simple-3.c
long y() { return 42; }
int main() { return &y; }
{% endhighlight %}

which generates:

{% highlight bash %}
$ objdump -d simple-3.o
Disassembly of section .text:

0000000000000000 <y>:
   0:   b8 2a 00 00 00          mov    $0x2a,%eax
   5:   c3                      retq   

Disassembly of section .text.startup:

0000000000000000 <main>:
   0:   b8 00 00 00 00          mov    $0x0,%eax
   5:   c3                      retq   
{% endhighlight %}

Notice that `main` encodes the address of `y` as simply 0. This will generate a segmentation fault at runtime, so how can it be correct?

In addition to the `.text` sections above, there are several other types of sections. One is a __relocation__ section, which provides a list of placeholder values that the linker needs to fill in later. Here's how to see the relocations in an object file:
{% highlight bash %}
$ readelf -r simple-3.o

Relocation section '.rela.text.startup' at offset 0x208 contains 1 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
000000000001  000a0000000a R_X86_64_32       0000000000000000 y + 0
{% endhighlight %}

In our `main` function, the 4-byte address of `y` starts at offset 1, so there is a 32-bit relocation located at offset 1. The relocation includes the intended symbol name, which the linker can use when matching names in different files.

You could use a linker to remove this relocation, or use a different name so that `gcc` doesn't place it in a different section:
{% highlight cpp %}
// gcc -O2 -c simple-3.c
long y() { return 42; }
int custom_main() { return &y; }
{% endhighlight %}

Now they've been merged into the same section, but there is still a relocation in `custom_main`:
{% highlight cpp %}
Disassembly of section .text:

0000000000000000 <y>:
   0:   b8 2a 00 00 00          mov    $0x2a,%eax
   5:   c3                      retq   
   6:   66 2e 0f 1f 84 00 00    nopw   %cs:0x0(%rax,%rax,1)
   d:   00 00 00 

0000000000000010 <custom_main>:
  10:   b8 00 00 00 00          mov    $0x0,%eax
  15:   c3                      retq   
{% endhighlight %}

Using the -fPIC` flag will generate __Position-Independent__ code, and finally eliminates this relocation:
{% highlight cpp %}
// gcc -O2 -c simple-3.c -fPIC
static long y() { return 42; }
int custom_main() { return &y; 
{% endhighlight %}

{% highlight bash %}
0000000000000000 <y>:
   0:   b8 2a 00 00 00          mov    $0x2a,%eax
   5:   c3                      retq   
   6:   66 2e 0f 1f 84 00 00    nopw   %cs:0x0(%rax,%rax,1)
   d:   00 00 00 

0000000000000010 <custom_main>:
  10:   48 8d 05 e9 ff ff ff    lea    -0x17(%rip),%rax        # 0 <y>
  17:   c3                      retq   
{% endhighlight %}

Notice that I used the `static` modifier on `y`. This allows `gcc` to conclude that `y` will not be replaced at link-time with another function with the same name, which allows `-fPIC` to generate an instruction-relative address.

#### Jump Tables

A developer practicing Test-Driven Development has provided the following definition of the nth prime number:

{% highlight cpp %}
// gcc -Os simple-4.c -c -fPIC
int prime(int n) {
  switch (n) {
  case 1: return 2;
  case 2: return 3;
  case 3: return 5;
  case 4: return 7;
  case 5: return 11;
  case 6: return 13;
  case 7: return 17;
  case 8: return 19;
  default: __builtin_trap();
  }
}
{% endhighlight %}

Unfortunately, `gcc` has placed the jump table within an `.rodata` section and left a relocation in `.text`. The relocation causes a segmentation fault at runtime:
{% highlight bash %}
$ objdump -d simple-4.o
Disassembly of section .text:

0000000000000000 <prime>:
   0:   ff cf                   dec    %edi
   2:   83 ff 07                cmp    $0x7,%edi
   5:   77 34                   ja     3b <prime+0x3b>
   7:   48 8d 05 00 00 00 00    lea    0x0(%rip),%rax        # e <prime+0xe>
   e:   48 63 14 b8             movslq (%rax,%rdi,4),%rdx
  12:   48 01 d0                add    %rdx,%rax
  15:   ff e0                   jmpq   *%rax
  17:   b8 03 00 00 00          mov    $0x3,%eax
  1c:   c3                      retq   
  1d:   b8 07 00 00 00          mov    $0x7,%eax
  22:   c3                      retq   
  23:   b8 0b 00 00 00          mov    $0xb,%eax
  28:   c3                      retq   
  29:   b8 0d 00 00 00          mov    $0xd,%eax
  2e:   c3                      retq   
  2f:   b8 11 00 00 00          mov    $0x11,%eax
  34:   c3                      retq   
  35:   b8 13 00 00 00          mov    $0x13,%eax
  3a:   c3                      retq   
  3b:   0f 0b                   ud2    
  3d:   b8 02 00 00 00          mov    $0x2,%eax
  42:   c3                      retq   
  43:   b8 05 00 00 00          mov    $0x5,%eax
  48:   c3                      retq

$ readelf --relocs simple-4.o

Relocation section '.rela.text' at offset 0x238 contains 1 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
00000000000a  000600000002 R_X86_64_PC32     0000000000000000 .rodata - 4

Relocation section '.rela.rodata' at offset 0x250 contains 8 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
000000000000  000200000002 R_X86_64_PC32     0000000000000000 .text + 3d
000000000004  000200000002 R_X86_64_PC32     0000000000000000 .text + 1b
000000000008  000200000002 R_X86_64_PC32     0000000000000000 .text + 4b
00000000000c  000200000002 R_X86_64_PC32     0000000000000000 .text + 29
000000000010  000200000002 R_X86_64_PC32     0000000000000000 .text + 33
000000000014  000200000002 R_X86_64_PC32     0000000000000000 .text + 3d
000000000018  000200000002 R_X86_64_PC32     0000000000000000 .text + 47
00000000001c  000200000002 R_X86_64_PC32     0000000000000000 .text + 51
{% endhighlight %}

Not only is there a relocation to `.rodata` to get the jump table, but the jump table itself has relocations back to `.text`. It's possible to use a different jumptable in `prime`, or to use the jumptable in a different function; so the linker has to resolve two layers of indirection.

If we're willing to take a performance hit, `gcc` provides the `-fno-jump-tables` option to avoid this issue:
{% highlight cpp %}
0000000000000000 <prime>:
   0:   83 ff 04                cmp    $0x4,%edi
   3:   74 30                   je     35 <prime+0x35>
   5:   7f 10                   jg     17 <prime+0x17>
   7:   83 ff 02                cmp    $0x2,%edi
   a:   74 23                   je     2f <prime+0x2f>
   c:   7f 3f                   jg     4d <prime+0x4d>
   e:   b8 02 00 00 00          mov    $0x2,%eax
  13:   ff cf                   dec    %edi
  15:   eb 14                   jmp    2b <prime+0x2b>
  17:   83 ff 06                cmp    $0x6,%edi
  1a:   74 25                   je     41 <prime+0x41>
  1c:   7c 1d                   jl     3b <prime+0x3b>
  1e:   83 ff 07                cmp    $0x7,%edi
  21:   74 24                   je     47 <prime+0x47>
  23:   b8 13 00 00 00          mov    $0x13,%eax
  28:   83 ff 08                cmp    $0x8,%edi
  2b:   74 25                   je     52 <prime+0x52>
  2d:   0f 0b                   ud2    
  2f:   b8 03 00 00 00          mov    $0x3,%eax
  34:   c3                      retq   
  35:   b8 07 00 00 00          mov    $0x7,%eax
  3a:   c3                      retq   
  3b:   b8 0b 00 00 00          mov    $0xb,%eax
  40:   c3                      retq   
  41:   b8 0d 00 00 00          mov    $0xd,%eax
  46:   c3                      retq   
  47:   b8 11 00 00 00          mov    $0x11,%eax
  4c:   c3                      retq   
  4d:   b8 05 00 00 00          mov    $0x5,%eax
  52:   c3                      retq

$ readelf --relocs simple-4.o
# No relocations emitted
{% endhighlight %}

If we want the jump table but don't want the relocations, we can always run a linker to resolve them for us. In a future article, maybe I'll show how one can write a custom linker to resolve simple relocations.

### Unsupported CPU Instructions

Our chess engine uses a lot of bitwise operators to manipulate bitsets. Unfortunately, `gcc` emits a call to a library function even at the highest optimization levels:
{% highlight cpp %}
// gcc -O3 -c simple-5.c
int count_bits(unsigned int x) { return __builtin_popcount(x); }

// objdump -d simple-5.o
0000000000000000 <count_bits>:
   0:   48 83 ec 08             sub    $0x8,%rsp
   4:   89 ff                   mov    %edi,%edi
   6:   e8 00 00 00 00          callq  b <count_bits+0xb>
   b:   48 83 c4 08             add    $0x8,%rsp
   f:   c3                      retq

// readelf --relocs simple-5.o
Relocation section '.rela.text' at offset 0x1e0 contains 1 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
000000000012  000800000002 R_X86_64_PC32     0000000000000000 __popcountdi2 - 4
{% endhighlight %}

Newer CPUs have instructions for counting the number of bits in a value, and more. We can tell gcc to use them using the `-march` parameter:
{% highlight cpp %}
// gcc -O3 -c simple-5.c -march=native
int count_bits(unsigned int x) { return __builtin_popcount(x); }

// objdump -d simple-5.o
0000000000000000 <count_bits>:
   0:   31 c0                   xor    %eax,%eax
   2:   f3 0f b8 c7             popcnt %edi,%eax
   6:   c3                      retq   
{% endhighlight %}

The Amazon Redshift node type that I'm testing on uses Intel Xeon E5-2670v2 CPUs, so you would set `-march=ivybridge`. By default, my `gcc` emits code for the `x86-64` target, whose specification was originally released in 2000. So make sure you're getting the full benefit of your newer CPU instructions.

## Using a Loader for Staging

Once the code has been built and any relocations removed, we'll want to test it locally before we load it into Redshift. Here is the actual entry point for our C code:
{% highlight cpp %}
// Used when generating pf_best_move
extern "C" void custom_main(const char *g_str, char *m_dest, int depth)
{
  gamestate g = parse_fen(g_str);
  move m = best_move(g, depth);
  if (! g.is_white)
    m = swap_move(m);
  print_move(m, m_dest);
}

gamestate parse_fen(const char* s);
move swap_move(move m);
void print_move(move m, char *buffer);
{% endhighlight %}

`g_str` is a FEN string representing the board state, `depth` is passed to the `negamax` function to get more accurate scoring, and `m_dest` is a buffer that we will write the string representation of the engine's move to.

After the `chess.c` file has been compiled, we need to extract the shellcode string and offset to `custom_main` into Python variables. Here's a Haskell script that will read the object file and print those:
{% highlight haskell %}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import Data.Elf
import System.Environment
import Data.ByteString as BS hiding (map)
import Data.ByteString.Builder as BSB
import Data.ByteString.Builder.Prim as BSB (primFixed)
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Semigroup
import Data.Word
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Prelude hiding (readFile)

main :: IO ()
main = do
  [filename] <- getArgs
  bs <- readFile filename
  let elf = parseElf bs
  dumpElf elf

dumpElf :: Elf -> IO ()
dumpElf elf = do
  let ss = elfSections elf
      sectionsByName = map (\s -> (elfSectionName s, s)) ss
  case lookup ".text" sectionsByName of
    Nothing -> error "No .text section found"
    Just s -> do
      -- Output actual shellcode
      BSL.putStr "shellcode = b'"
      dumpPythonBytes $ elfSectionData s
      BSL.putStr "'\n"
      -- Output offset to the "main" function
      let offset = getOffset elf
      BSL.putStr "offset = "
      BSL.putStr $ TL.encodeUtf8 $ TL.pack $ show offset
      BSL.putStr "\n"
      
  return ()

getOffset :: Elf -> Word64
getOffset elf =
  let stes = Prelude.concat $ parseSymbolTables elf
      textStes = [ x | x@EST { steEnclosingSection = Just (ElfSection { elfSectionName = ".text" }) } <- stes ]
      mainStes = [ x | x@EST { steName = (_, Just "custom_main")} <- textStes ]
      mainOs = Prelude.map steValue mainStes
  in case mainOs of
    [x] -> x
    [] -> error "No main functions found"
    _ -> error $ "Multiple main functions found: " ++ show mainStes
  
dumpPythonBytes :: ByteString -> IO ()
dumpPythonBytes bs = case BS.uncons bs of
  Nothing -> return ()
  Just (b, bs') -> do
    putByte b
    dumpPythonBytes bs'
  where
    putByte b = do
      BSL.putStr $ BSB.toLazyByteString $ "\\x" <> word8HexFixed b
{% endhighlight %}

For testing, I wrote the above output to a file named `shellcode_bytes.py` and included the line `from shellcode_bytes import shellcode, offset` in the Python loader.

The final SQL is built by pasting the above output into the corresponding loader:
{% highlight python %}
create or replace function pf_best_move (fen varchar, depth integer) returns varchar stable as $$
from ctypes import (CDLL, c_long, c_char_p, c_void_p, memmove, cast, Structure, CFUNCTYPE, c_uint64, c_int, create_string_buffer)

def execute():
    shellcode=???
    offset = ???
    
    libc = CDLL('libc.so.6')

    src_ptr = c_char_p(shellcode)
    size = len(shellcode)

    code_ptr = libc.valloc(size)
    code_ptr = c_void_p(code_ptr)

    memmove(code_ptr, src_ptr, size)
    err = libc.mprotect(code_ptr, size, 0x7)
    if 0 != err:
        raise Exception("mprotect: " + str(err))
    main_ptr = c_void_p(c_uint64(code_ptr.value).value + c_uint64(offset).value)
    fen_buf = c_char_p(fen)
    move_buf = create_string_buffer(10)

    SHELLCODE_T = CFUNCTYPE(None, c_char_p, c_char_p, c_int)
    
    fptr = cast(main_ptr, SHELLCODE_T)
    result = fptr(fen, move_buf, depth)
    libc.free(code_ptr)
    return move_buf.raw
return execute()
$$ language plpythonu;
{% endhighlight %}

Check out the [Github](https://github.com/MichaelBurge/redshift-shellcode/tree/master) repository for the full code.

# Conclusion

A chess engine was implemented and deployed to an Amazon Redshift node, and a sample game was played. If you want to learn more, I recommend checking out:

* [Chess Programming Wiki](https://chessprogramming.wikispaces.com/)
* [Amazon Redshift](https://aws.amazon.com/documentation/redshift/)

Future chess articles may cover:
* Using theorem provers to formally verify a bitboard-based chess implementation
* More advanced tree search algorithms using hashing and caching
* Using Machine Learning to tune the scoring function
* Using GPUs to accelerate move search
* More advanced scoring functions
* Reducing CPU time of my naive implementation by several orders of magnitude
* Using a graphical frontend like Fritz to play against your engine
* Designing engines for Chinese Chess, Shogi, or other variations

Future database articles may cover:
* Implementing a PostgreSQL-compatible database frontend
* Implementing a GPU-accelerated database
* How to design a database schema for efficient queries
* Using Redshift implementation details to design and optimize queries

Future assembly articles may cover:
* Writing simple programs directly in assembly
* Tweaking compiler flags to get good performance
* Debugging assembly with no source code or symbols
* Writing your own linker
* Writing your own Javascript JIT compiler
* Starting a Linux process by scribbling it directly into `/dev/mem`