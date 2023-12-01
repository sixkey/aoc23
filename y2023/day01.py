from aoc import *


def line_to_number( line ):
    numbers = dict_concat( number_chars, number_words )
    start = numbers[ str_findfirst( line, numbers.keys() ) ]
    end = numbers[ str_findfirst( line, numbers.keys(), range( len( line ), -1, -1 ) ) ]
    return start * 10 + end

@aoc_problem
@with_tensor_chain(
    tlines(),
    tmap( str_to_chrs ),
    tmap( kfilter( lambda c: c.isdigit() ) ),
    tmap( int, depth = 2 ),
    tmap( lambda l: l[0] * 10 + l[-1] )
)
def problem( text ):
    yield sum( text )

@aoc_problem
@with_tensor_chain(
    tlines(),
    tmap( line_to_number ),
)
def problem( lines ):
    yield sum( lines )
