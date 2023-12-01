### str #######################################################################


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
