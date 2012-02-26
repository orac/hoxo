# HOXO - a noughts-and-crosses player

I wrote this over a weekend to teach myself Haskell, so don't expect polish or
documentation. It plays noughts-and-crosses against the user, and prints out
cutesy messages that may or may not be related to WarGames. I present the code
not in the expectation that anyone will actually want to use it, but in case
anyone is curious. Please don't use it as a teaching aid as it is quite
obviously newbie code.

Compile the code with `ghc hoxo.hs`, or use ghci to `:load` it if you want more
of a play around. I didn't think it was worth providing a build-system for a
one-file project.

## The model

I decided to model the board as a list of lists. Each element is a tri-state
cell indicating who has played there. This is a pretty shitty representation,
and means I have !! all over the place, but it was simple and suits the length of
the code. I considered a Data.Array with the indices being (Int, Int) pairs, but
that was pretty hideous too. The board size and shape can actually be changed
pretty easily by editing the `newBoard` function/constant, but if you do this you
need to update `scoreSpan` as well.

The score is modelled as a triplet of integers. The first element gives the
number of winning lines (i.e. three of the same symbol); the second element is
the number of lines containing two of the same symbol and a space; the third,
one of the same symbol and two spaces. I picked the convention that wins for X
are positive and wins for O negative.

## The algorithm

It's a pretty trivial traversal of the game tree. I started off limiting it by
depth but on a game this simple there's no advantage to that. It does min-max to
find the best ultimate score for each move, sorts them, and picks the best. I
was pleased to note that by default triplets sort in lexicographic order, so I
didn't need to pick weighting factors to produce a single integer for the
score, with the accompanying risk of accidentally making two runs of two better
than a win.

## The interface

The solver can actually play as O or X, depending on whether `ioLoop` calls
`humanFirst` or `computerFirst`, but I didn't bother offering an interface to
that choice. The interface to the solver is a list of characters, each one being
a digit 1-9 indicating the board position corresponding to the numpad position
of that digit. Rudimentary input checking is performed. The front-end is just
that wrapped with `interact`. Thanks to lazy evaluation, the algorithm runs
until it needs the next character, and then waits for input, without having to
write any special code to deal with that, which I thought was rather slick.

## The licence

MIT licence. I'm not publishing this in the expectation of getting anything out
of it, but if you find it useful, or reuse some of the code or ideas, I'd be
really happy if you tell me what you've used it for.
