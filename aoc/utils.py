### str #######################################################################


def lpad( string, chars, padding_char = " " ):
    if not isinstance( string, str ):
        string = str( string )
    return padding_char * ( chars - len( string ) ) + string
