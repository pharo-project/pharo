private building
printStringOf: anObject
	| className |
	className := anObject class name.
	^ (String streamContents: [ :s |
		s
			nextPutAll: (className first isVowel ifTrue: ['an '] ifFalse: ['a ']);
			nextPutAll: className ]) asComment