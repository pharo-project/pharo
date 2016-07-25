accessing
targetClassName

	^targetClass isSymbol
		ifTrue: [ targetClass ]
		ifFalse: [ targetClass name ]