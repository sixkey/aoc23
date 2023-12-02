from aoc import *
from collections import defaultdict

def check_count( counter ):
    return ( counter[ 'red' ] <= 12
         and counter[ 'green' ] <= 13
         and counter[ 'blue' ] <= 14 )


def count_set( st ):
    counter = defaultdict( int )
    for c, color in st:
        counter[ color ] += int( c )
    return check_count( counter )


def power_set( st ):
    counter = dict()
    for c, color in st:
        if color not in counter or counter[ color ] < int( c ): counter[ color ] = int( c )
    return counter[ 'red' ] * counter[ 'green' ] * counter[ 'blue' ]


def parse_line( line ):
    return [ [ t.strip().split(' ') for t in r.split(',') ] for r in line.split(';') ]


@aoc_problem
@with_tensor_chain(
    tlines(),
    tmap( lambda s: s.split(':') ),
    tbimap( kmatch( 'Game (\d+)' )( int ),
            lambda r: ( lambda x : [ kall( [ count_set( r ) for r in x ] )
                                   , power_set( list_concat( *x ) ) ] )( parse_line( r ) ) )
)
def problem( lines ):
    yield sum( i for i, [ b, _ ] in lines if b )
    yield sum( x for _, [ _, x ] in lines )
