accessing
nextPut: aCharacter

	(String crlf includes: aCharacter )
		ifFalse: [ 
			previous ifNotNil: [ self newLine ].
			^ stream nextPut: aCharacter ].

	previous = Character cr
		ifTrue: [ self newLine ].
	aCharacter ~= Character lf
		ifTrue: [ previous := aCharacter ].