accessing
enumerateTo: aSet ignoringCase: aBoolean

	aBoolean
		ifTrue: 
			[aSet 
				add: character asUppercase;
				add: character asLowercase]
		ifFalse: [aSet add: character]