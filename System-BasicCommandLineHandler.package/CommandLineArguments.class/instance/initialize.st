initialization
initialize
	
	| documentPath  |
	
	arguments := OrderedCollection withAll: Smalltalk arguments.
	documentPath := Smalltalk vm documentPath.
	documentPath isEmptyOrNil
		ifFalse: [ arguments addFirst: documentPath ].