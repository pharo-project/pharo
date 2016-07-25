identifier testing
isIncompleteBlockTempName: aString 
	| temp |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			temp := (temporaries
						at: level
						ifAbsent: [#()])
						anySatisfy: [:each | each beginsWith: aString].
			temp
				ifTrue: [^ true]].
	^ false