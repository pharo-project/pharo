collection ops
includes: aCharacter
	aCharacter asciiValue >= 256
		ifTrue: ["Guard against wide characters"
			^false].
	^(map at: aCharacter asciiValue + 1) > 0