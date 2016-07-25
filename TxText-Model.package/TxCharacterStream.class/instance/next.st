accesing
next
	| char |
	
	[ 
		position := position nextPosition.
		char := position getCharacter.
		char ifNotNil: [ ^ char ].
		position isAtEnd ifTrue: [ ^ nil ].
	] repeat.