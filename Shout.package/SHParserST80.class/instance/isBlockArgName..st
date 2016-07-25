identifier testing
isBlockArgName: aString 
	| temp arg |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			arg := (arguments
						at: level
						ifAbsent: [#()])
						includes: aString.
			arg
				ifTrue: [^ true].
			temp := (temporaries
						at: level
						ifAbsent: [#()])
						includes: aString.
			temp
				ifTrue: [^ false]].
	^ false