from aoc import *

def find_symbols( mat, symbols = None ):
    def is_symbol( sym ):
        return ( not sym.isdigit()
             and sym != '.'
             and ( symbols is None or sym in symbols ) )
    for row in range( mat.shape[ 0 ] ):
        for col in range( mat.shape[ 1 ] ):
            if is_symbol( mat[ row, col ] ):
                yield row, col


def find_neigh_numbers( mat ):
    seen = tensor_like( mat, '.', depth = 2 )
    for sy, sx in find_symbols( mat ):
        for ny, nx in eight_neighbours( sy, sx, 0, mat.shape[ 0 ] - 1, 0, mat.shape[ 1 ] - 1 ):
            if not mat[ ny, nx ].isdigit(): continue
            end = nx
            while end < mat.shape[ 1 ] and mat[ ny, end ].isdigit():
                seen[ ny, end ] = mat[ ny, end ]
                end += 1
            start = nx
            while start >= 0 and mat[ ny, start ].isdigit():
                seen[ ny, start ] = mat[ ny, start ]
                start -= 1
    return seen

def neighbour_numbers( mat, sy, sx ):
    seen = set()
    for ny, nx in eight_neighbours( sy, sx, 0, mat.shape[ 0 ] - 1, 0, mat.shape[ 1 ] - 1 ):
        if not mat[ ny, nx ].isdigit(): continue
        end = nx
        while end < mat.shape[ 1 ] and mat[ ny, end ].isdigit():
            end += 1
        start = nx
        while start >= 0 and mat[ ny, start - 1 ].isdigit():
            start -= 1
        current = set( ( ny, i ) for i in range( start, end ) )
        if current.intersection( seen ):
            continue
        seen |= current
        yield int( "".join( mat[ ny, i ] for i in range( start, end ) ) )

@tpipe
def find_neighbours( mat, sym ):
    def go():
        for sy, sx in find_symbols( mat, sym ):
            yield list( neighbour_numbers( mat, sy, sx ) )
    return Tensor( go() )

@aoc_problem
@with_tensor_chain(
    tlines(),
    tmap( str_to_chrs ),
    tomat(),
    find_neigh_numbers,
    tmap( kjoin( "" ), depth = 1 ),
    tmap( kmatch( "([^\d]*(\d+)[^\d]*)*", ( 2, ) )( list ) ),
    tmap( int, depth = 2 )
)
def problem_a( mat ):
    yield sum( mat( 2 ) )

@aoc_problem
@with_tensor_chain(
    tlines(),
    tmap( str_to_chrs ),
    tomat(),
    find_neighbours( [ "*" ] )
)
def problem_b( gear_numbers ):
    res = 0
    for adjacent in gear_numbers:
        print( adjacent )
        if len( adjacent ) == 2:
            res += adjacent[0] * adjacent[1]
    yield res

