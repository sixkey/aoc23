### str #######################################################################


from functools import reduce
from itertools import chain
from operator import __and__
import regex


def lpad( string, chars, padding_char = " " ):
    if not isinstance( string, str ):
        string = str( string )
    return padding_char * ( chars - len( string ) ) + string

def kfilter( fun ):
    def kfilter_f( data ):
        return data.__class__( filter( fun, data ) )
    return kfilter_f

number_chars = { '0': 0,
                 '1': 1,
                 '2': 2,
                 '3': 3,
                 '4': 4,
                 '5': 5,
                 '6': 6,
                 '7': 7,
                 '8': 8,
                 '9': 9 }

number_words = { 'zero': 0,
                 'one': 1,
                 'two': 2,
                 'three': 3,
                 'four': 4,
                 'five': 5,
                 'six': 6,
                 'seven': 7,
                 'eight': 8,
                 'nine': 9 }

def str_findfirst( line, words, index_range = None ):
    for i in index_range if index_range is not None else range( len( line ) ):
        for w in words:
            if line[ i: ].startswith( w ):
                return w
    return None

def dict_concat( *args ):
    result = {}
    for arg in args:
        result.update( arg )
    return result

def list_concat( *args ):
    result = []
    for arg in args:
        result += arg
    return result

def kall( *args ):
    if len( args ) == 1:
        return reduce( __and__, args[ 0 ], True )
    return reduce( __and__, args, True )

def kmatch( pattern, capture = None ):
    re_pattern = regex.compile( pattern )
    def kmatch_d( fun ):
        def kmatch_w( string ):
            result = re_pattern.match( string )
            if result is None:
                return None
            if capture is None:
                return fun( result.groups )
            else:
                return fun( *( result.captures(i) for i in capture ) )
        return kmatch_w
    return kmatch_d

def ksnd( x ):
    return x[1]

def kfst( x ):
    return x[1]

def eight_neighbours( y, x, miny = None, maxy = None, minx = None, maxx = None ):
    def _go():
        yield y - 1, x - 1
        yield y - 1, x
        yield y - 1, x + 1
        yield y, x - 1
        yield y, x + 1
        yield y + 1, x - 1
        yield y + 1, x
        yield y + 1, x + 1
    def in_box( yy, xx ):
        return ( ( minx is None or minx <= xx )
             and ( maxx is None or xx <= maxx )
             and ( miny is None or miny <= yy )
             and ( maxy is None or yy <= maxy ) )
    for yy, xx in _go():
        if in_box( yy, xx ):
            yield yy, xx


def kjoin( s ):
    def fun( *args ):
        return s.join( chain( *( map( lambda x: str( x ), arg ) for arg in args ) ) )
    return fun
