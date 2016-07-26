accessing
actualClass
	^ Smalltalk globals
		at: className
		ifPresent: [ :class | 
			classIsMeta
				ifTrue: [ class classSide ]
				ifFalse: [ class ] ]