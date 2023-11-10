from __future__ import annotations

from typing import Iterable, Union
from aoc import aoc_problem
from copy import copy


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


    def __iter__( self ):
        return self.iter( 1 )


    def __call__( self, depth = 1 ):
        if depth == 0: return self
        return self.iter( depth )


TensorT = Union[ 'Tensor', TensorView ]



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


class TensorIterator:


    def __init__( self, matrix, depth, index_prefix = None, end = None ):
        self.matrix = matrix

        prefix_depth = len( index_prefix ) if index_prefix is not None else 0
        depth = depth + prefix_depth

        self.local_depth = min( depth, matrix.dim() )

        self.local_index = [ 0 for i in range( self.local_depth ) ]
        if index_prefix is not None:
            self.local_prefix = index_prefix + self.local_index

        self.child_depth = max( 0, depth - self.local_depth )
        self.child_iterator = None

        self.end = end


    def __iter__( self ):
        return self

    def increment_index( self ):
        self.local_index = increment_index( self.matrix, self.local_index )
        if ( self.local_index is not None
               and self.end is not None
               and self.local_index[ : len( self.end ) ] == self.end ):
            return None
        return self.local_index

    def __next__( self ):

        print( self.local_index )

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
        self.child_iterator = iter( item )
        return next( self )


class Tensor( Iterable ):


    def __init__( self, elements, shape ):
        self.elements = elements
        self.shape = shape
        self.shape_sizes = _shape_sizes( shape )


    def dim( self ):
        return len( self.shape )


    def __getitem__( self, index ):
        index = indexify( index )
        real_index = 0
        for i, s in zip( index, self.shape_sizes ):
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
        if ( depth == 0 ): return self
        return self.iter( 1 )


def with_lines( filename: str ):
    with open( filename, "r" ) as f:
        return Tensor( ( l.strip() for l in f ), (1,) )


def tchain( tensor, *tensor_pipes ):
    current = tensor
    for tensor_pipe in tensor_pipes:
        current = tensor_pipe( current )
    return current
