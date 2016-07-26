system attribute
options
	"Answer an array with all the command line options."

	"Smalltalk commandLine options"

	^ Array
		streamContents: [ :str | 
			| arg i |
			i := 1.
			[ i > 1000 or: [ (arg := self optionAt: i) isNil ] ]
				whileFalse: [ 
					str nextPut: arg.
					i := i + 1 ] ]