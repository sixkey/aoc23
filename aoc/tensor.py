from __future__ import annotations
import functools

from typing import Iterable, Union
from copy import copy
from .utils import lpad

def _shape_sizes( shape ):
    sizes = []
    size = 1
    for s in reversed( shape ):
        sizes.append( size )
        size *= s
    sizes.reverse()
    return sizes


def indexify( index ):
    if isinstance( index, int ): return [ index ]
    return index


class TensorView:


    def __init__( self, tensor, start, end ):
        self.start = start
        self.end = end
        self.tensor = tensor


    def __getitem__( self, index ):
        return self.tensor[ self.start + indexify( index ) ]


    def __setitem__( self, index, value ):
        self.tensor[ self.start + indexify( index ) ] = value


    def iter( self, depth ):
        return TensorIterator( self.tensor
                             , depth
                             , index_prefix = self.start
                             , end = self.end )

    def dim( self ):
        return self.tensor.dim() - len( self.start )


    def __iter__( self ):
        return self.iter( 1 )


    def __call__( self, depth = 1 ):
        if depth in [ 'all', 'everything', -1 ]:
            return self.iter( self.dim() )
        if depth == 0:
            return self
        return self.iter( depth )

    def shape_sizes( self, i ):
        return self.tensor.shape_sizes[ len( self.start ) + i ]


class Tensor( Iterable ):


    def __init__( self, elements, shape = None ):
        self.elements = list( elements )
        self.shape = shape if shape is not None else (len(self.elements),)
        self._shape_sizes = _shape_sizes( self.shape )


    def dim( self ):
        return len( self.shape )

    def shape_sizes( self, i ):
        return self._shape_sizes[ i ]



    def __getitem__( self, index ):
        index = indexify( index )
        real_index = 0
        for i, s in zip( index, self._shape_sizes ):
            real_index += i * s
        if len( index ) < len( self.shape ):
            return TensorView( self, index, increment_index( self, index ) )
        if len( index ) > len( self.shape ):
            return self.elements[ real_index ][ index[ len( self.shape ): ] ]
        return self.elements[ real_index ]


    def iter( self, depth ):
        return TensorIterator( self, depth )


    def __iter__( self ):
        return self.iter( 1 )


    def __str__( self ):
        return "[" + ", ".join( e.__str__() for e in self.elements ) + "]"


    def __call__( self, depth = 1 ):
        if ( depth in [ "all", "everything", -1 ] ):
            return self.iter( self.dim() )
        if ( depth == 0 ):
            return self
        return self.iter( depth )


    def map( self, fun, depth = None ):

        if depth is None:
            depth = self.dim()

        if depth == 0:
            return fun( self )

        if depth <= self.dim():
            return Tensor( ( fun( e ) for e in self( depth ) ), self.shape[:depth] )

        return Tensor( ( deepmap( e, fun, depth - self.dim() ) for e in self.elements), self.shape )


TensorT = Union[ Tensor, TensorView ]


def increment_index( tensor, index ):
    ind = copy( index )
    ind[ -1 ] += 1
    for i in range( len( ind ) - 1, -1, -1 ):
        if ind[ i ] >= tensor.shape[ i ]:
            ind[ i ] = 0
            if i == 0:
                return None
            ind[ i - 1 ] += 1
    return ind


class DeepIterator:


    def __init__( self, iterated_thing, depth ):
        self.iterated_thing = iterated_thing
        self.child_iterator = None
        self.shallow_iterator = iter( iterated_thing )
        self.depth = depth


    def __next__( self ):
        if self.child_iterator is not None:
            try:
                return next( self.child_iterator )
            except StopIteration:
                pass
        child = next( self.shallow_iterator )
        self.child_iterator = deep_iterator( child, self.depth - 1 )
        return next( self )


def deep_iterator( element, depth ):

    assert depth > 0

    if isinstance( element, TensorT ):
        return element.iter( depth )

    if depth == 1:
        return iter( element )

    return DeepIterator( element, depth )


class TensorIterator:


    def __init__( self, matrix, depth, index_prefix = None, end = None ):

        self.matrix = matrix

        prefix_depth = len( index_prefix ) if index_prefix is not None else 0
        depth = depth + prefix_depth

        self.local_depth = min( depth, matrix.dim() )

        self.local_index = [ 0 for i in range( self.local_depth ) ]
        if index_prefix is not None:
            for i, v in enumerate( index_prefix ):
                self.local_index[ i ] = index_prefix[ i ]

        self.child_depth = max( 0, depth - self.local_depth )
        self.child_iterator = None

        self.end = end


    def __iter__( self ):
        return self



    def increment_index( self ):
        self.local_index = increment_index( self.matrix, self.local_index )

        if ( self.local_index is not None
               and self.end is not None
               and list_prefix( self.end, self.local_index ) ):
            self.local_index = None
        return self.local_index

    def __next__( self ):
        # the index is pointing behind the currently iterated child
        if self.child_iterator is not None:
            try:
                return next( self.child_iterator )
            except StopIteration:
                pass

        if self.local_index is None:
            raise StopIteration

        item = self.matrix[ self.local_index ]

        if self.child_depth == 0:
            self.increment_index()
            return item

        if not isinstance( item, TensorT ):
            raise RuntimeError( f"cannot iterate element {item}" )

        self.increment_index()
        self.child_iterator = deep_iterator( item, self.child_depth )
        return next( self )


def deepmap( item, fun, depth ):

    if depth == 0:
        return fun( item )

    if depth == 1 and not isinstance( item, TensorT ):
        return item.__class__( map( fun, item ) )


    if not isinstance( item, TensorT ):
        return item.type()( map( lambda x: deepmap( x, fun, depth - 1 ), item ) )

    return item.map( fun, depth )


def tpipe( fun ):
    @functools.wraps( fun )
    def tpipe_f( *args, **kwargs ):
        @functools.wraps( fun )
        def pipe( tensor ):
            return fun( tensor, *args, **kwargs )
        return pipe
    return tpipe_f


def tchain( tensor, *tensor_pipes ):
    current = tensor
    for tensor_pipe in tensor_pipes:
        current = tensor_pipe( current )
    return current


@tpipe
def tlines( filename: str ):
    with open( filename, "r" ) as f:
        return Tensor( list( l.strip() for l in f ) )


@tpipe
def tbimap( tensor, left, right ):
    return tmap( lambda e: [ left(e[0]), right(e[1]) ] )( tensor )

@tpipe
def tmap( tensor, fun, depth = 1 ):
    return deepmap( tensor, fun, depth )


@tpipe
def tfold( tensor, operator, start ):
    return functools.reduce( operator, tensor.iter( tensor.dim() ), start )


@tpipe
def tzipmap( tensor, *funs ):
    tensors = []
    counter = 0
    for element in tensor:
        if counter < len( funs ):
            tensors.append( funs[ counter ]( element ) )
            counter += 1
        else:
            tensors.append( element )
    return Tensor( tensors )


@tpipe
def tsplit( tensor, *lengths, repeat = False ):

    els = list( tensor( 'all' ) )

    if sum( lengths ) != tensor.shape[ 0 ]:
        raise RuntimeError( f"tensor with shape {tensor.shape} cannot be split into {lengths}" )

    tensors = []

    index = 0

    for l in lengths:
        new_shape = list( tensor.shape )
        new_shape[ 0 ] = l
        tensors.append( Tensor( els[ index * tensor.shape_sizes( 0 )
                                   : ( index + l ) * tensor.shape_sizes( 0 ) ]
                              , new_shape ) )
        index += l

    return Tensor( tensors )


def kapply( fun, *args, **kwargs ):
    return lambda o: fun( o, *args, **kwargs )


def kcast( constructor, are_args = False ):
    if are_args: return lambda e: constructor( *e )
    return lambda e: constructor( e )


def str_to_chrs( string ):
    return [ c for c in string ]


def list_prefix( a, b ):
    if len( a ) > len( b ): return False
    for i in range( len( a ) ):
        if a[ i ] != b[ i ]: return False
    return True


def draw_tensor( tensor ):

    def draw_line( depth, obj ):
        print( " " * depth + str( obj ) )

    def draw_lines( depth, lines ):
        for l in lines.splitlines():
            print( " " * depth + l )

    def draw_tensor_go( current, depth = 0 ):
        draw_line( depth, "[" )

        if isinstance( current, TensorT ) and current.dim() == 2:
            str_mat = tmap( str, 2 )( current )
            str_len = tmap( len, 2 )( str_mat )
            lpad_by = tfold( max, 0 )( str_len ) + 2
            draw_lines( depth, "\n".join(
                "".join( lpad( x, lpad_by ) for x in row )
                    for row in current ) )

        elif isinstance( current, TensorT ):
            for element in current:
                draw_tensor_go( element, depth + 1 )
        else:
            draw_line( depth, current )

        draw_line( depth, "]" )

    draw_tensor_go( tensor, 0 )

def draw_matrix( matrix, hor_delim = "", ver_delim = "\n", lpad_val = None ):
    if matrix.dim() != 2: raise RuntimeError( f"dimension of {matrix} is {matrix.dim()} not 2" )
    return ver_delim.join(
        hor_delim.join( str( x ) if lpad_val is None else lpad( x, lpad_val ) for x in row )
            for row in matrix )


## decorators #################################################################


def with_tensor_chain( *chain ):
    def with_tensor_chain_d( fun ):
        @functools.wraps(fun)
        def with_tensor_chain_w( argument ):
            return fun( tchain( argument, *chain ) )
        return with_tensor_chain_w
    return with_tensor_chain_d


