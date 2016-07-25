collection ops
remove: aCharacter
	aCharacter asciiValue >= 256
		ifFalse: ["Guard against wide characters"
			map at: aCharacter asciiValue + 1 put: 0].
	^aCharacter