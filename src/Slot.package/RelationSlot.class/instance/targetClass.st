accessing
targetClass

	targetClass isSymbol
		ifTrue: [ 
			targetClass := Smalltalk globals at: targetClass 
				ifAbsent: [ self error: 'Cannot find class ', targetClass printString ] ].
	
	^targetClass