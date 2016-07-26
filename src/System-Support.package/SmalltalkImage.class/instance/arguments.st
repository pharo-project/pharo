system attribute
arguments
	"Answer an array with all the command line arguments.
	This does not include imagePath, documentPath nor any option."

	"Smalltalk commandLine arguments"

	^ Array
		streamContents: [ :str | 
			| arg i |
			i := 1.
			[ i > 998 or: [ (arg := self argumentAt: i) isNil ] ]
				whileFalse: [ 
					str nextPut: arg.
					i := i + 1 ] ]