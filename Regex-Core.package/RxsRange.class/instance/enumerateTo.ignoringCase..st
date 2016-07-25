accessing
enumerateTo: aSet ignoringCase: aBoolean
	"Add all of the elements I represent to the collection."

	first asInteger to: last asInteger do:
		[:charCode | | character |
		character := charCode asCharacter.
		aBoolean
		ifTrue: 
			[aSet 
				add: character asUppercase;
				add: character asLowercase]
		ifFalse: [aSet add: character]]