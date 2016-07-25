identifier testing
isIncompleteBlockArgName: aString 
	| arg |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			arg := (arguments
						at: level
						ifAbsent: [#()])
						anySatisfy: [:each | each beginsWith: aString].
			arg
				ifTrue: [^ true]].
	^ false