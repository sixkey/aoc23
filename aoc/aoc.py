from functools import partial, singledispatch, wraps
from dataclasses import dataclass
import functools
from sys import argv
import os.path
import re
import requests
from typing import Callable, Any
from .utils import lpad
import numpy as np

### misc. decorator stuff #####################################################


def named_function( name: str ):
    def named_function_d( fun ):
        setattr( fun, "__name__", name )
        setattr( fun, "__qualname__", name )
        return fun
    return named_function_d


### numbers ###################################################################

def digits( number, base = 10 ):
    arr = []
    while number > 0:
        arr.append( number % base )
        number //= base
    arr.reverse()
    return arr


### aoc #######################################################################


def aoc_filename( year: int, day: int, suffix = "" ):
    return f"data/{year}_{ lpad( day, 2, padding_char = '0' ) }{suffix}.txt"


def aoc_get_input( day: int, year: int, cookie, overwrite: bool = False ):
    filename = aoc_filename( year, day )

    os.makedirs( f"data", exist_ok = True )

    if overwrite is False and os.path.exists( filename ):
        return filename

    response = requests.get(
            f"https://adventofcode.com/{year}/day/{day}/input",
            cookies={"session": cookie})
    if not response.ok:
        if response.status_code == 404:
            raise FileNotFoundError(response.text)
        raise RuntimeError(f"Request failed, code: {response.status_code}, message: {response.content}")
    with open( filename, "w" ) as f:
        f.write( response.text[:-1] )
    return filename


def load_cookie():
    with open( ".aoc" ) as f:
        return f.read().strip()


def _aoc_problem( year, day ):
    def aoc_problem_d( fun ):

        cookie = load_cookie()
        filename = aoc_get_input( day, year, cookie )
        for result in fun( filename ):
            print( result )
    return aoc_problem_d


def _get_day( filename: str ):
    act_name, _ = os.path.splitext( os.path.basename( filename ) )
    m = re.match( r"day(\d+)", act_name )
    if m is None:
        raise RuntimeError( f"{filename} is not a correct solution name" )
    return int( m.group(1) )


def _aoc_problem_dispatch( fun, *args ):
    day = _get_day( argv[ 0 ] ) if len( args ) <= 1 else args[ 1 ]
    if isinstance( args[ 0 ], int ):
        return fun( args[ 0 ], day  )
    else:
        return fun( 2023, day )( args[ 0 ] )


def aoc_problem( *args ):
    return _aoc_problem_dispatch( _aoc_problem, *args )


def _aoc_problem_test( year, day ):
    def _aoc_problem_test( fun ):
        filename = aoc_filename( year, day, "_test" )
        if not os.path.exists( filename ):
            raise RuntimeError( f"file {filename} does not exists" )
        for result in fun( filename ):
            print( result )
    return _aoc_problem_test


def aoc_problem_test( *args ):
    return _aoc_problem_dispatch( _aoc_problem_test, *args )


def aoc_test( fun ):
    for result in fun( "test.txt" ):
        print( result )


def gbrick( fun ):
    @named_function( fun.__name__ )
    def gbrick_f( *args ):
        @named_function( fun.__name__ + "_gbrick" )
        def gbrick_f_b( gen ):
            yield from fun( *args, gen )
        setattr( gbrick_f_b, "__name__", "foo" )
        return gbrick_f_b
    return gbrick_f


def decorify_gbrick( gbrick ):
    @named_function( "d_" + gbrick.__name__ )
    def gbrick_w( *args ):
        def gbrick_d( fun ):
            def gbrick_f( gen ):
                yield from fun( gbrick( *args )( gen ) )
            return gbrick_f
        return gbrick_d
    return gbrick_w


### decorators ################################################################

@gbrick
def gmap( fun, gen ):
    for element in gen:
        yield fun( element )


d_gmap = decorify_gbrick( gmap )


def d_with_file( fun ):
    def with_file_f( filename: str ):
        with open( filename, "r" ) as f:
            yield from fun( f )
    return with_file_f

def d_with_lines( fun ):
    def with_lines_f( filename: str ):
        with open( filename, "r" ) as f:
            yield from fun( ( l.strip() for l in f ) )
    return with_lines_f


@gbrick
def blocks( num: int, gen ):
    counter = 0
    buffer = []
    for line in gen:
        if len( buffer ) >= num:
            yield tuple( buffer )
            buffer = []
        buffer.append( line )
    yield tuple( buffer )

d_blocks = decorify_gbrick( blocks )


def cast_to( constructor, element ):
    if isinstance( element, tuple ):
        return constructor( *element )
    else:
        return constructor( element )


@gbrick
def g_cast_to( constructor, gen ):
    for element in gen:
        yield cast_to( constructor, element )


d_cast_to = decorify_gbrick( g_cast_to )


###############################################################################


def d_stream( list ):
    def d_stream_d( fun ):
        yield from fun( list )
    return d_stream_d


def d_collect( constructor ):
    def d_collect_d( fun ):
        def d_collect_w( gen ):
            yield from fun( constructor( gen ) )
        return d_collect_w
    return d_collect_d


###############################################################################


def vec( *args ): return np.array( list( args ) )
