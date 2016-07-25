testing
commandLineArguments
	"self commandLineArguments"
	
	| documentPath args |
	
	args := OrderedCollection withAll: Smalltalk arguments.
	documentPath := Smalltalk vm documentPath.
	documentPath isEmptyOrNil
		ifFalse: [ args addFirst: documentPath ].
 	^ args.