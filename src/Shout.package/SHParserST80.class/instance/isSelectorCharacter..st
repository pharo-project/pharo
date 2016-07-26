character testing
isSelectorCharacter: aCharacter 
	aCharacter isAlphaNumeric
		ifTrue: [^ false].
	aCharacter isSeparator
		ifTrue: [^ false].
	('"#$'':().;[]{}^_' includes: aCharacter)
		ifTrue: [^ false].
	aCharacter asciiValue = 30
		ifTrue: [^ false].
	aCharacter asciiValue = 0
		ifTrue: [^ false].
	^ true