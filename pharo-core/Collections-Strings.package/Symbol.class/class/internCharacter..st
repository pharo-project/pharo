instance creation
internCharacter: aCharacter
	aCharacter asciiValue > 256 ifTrue:[^self intern: aCharacter asString].
	OneCharacterSymbols ifNil: [^self intern: aCharacter asString].
	^OneCharacterSymbols at: aCharacter asciiValue + 1
