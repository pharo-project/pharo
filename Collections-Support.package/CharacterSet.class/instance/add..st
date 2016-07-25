collection ops
add: aCharacter
	"I automatically become a WideCharacterSet if you add a wide character to myself"
	
	aCharacter asciiValue >= 256
		ifTrue: [| wide |
			wide := WideCharacterSet new.
			wide addAll: self.
			wide add: aCharacter.
			self becomeForward: wide.
			^aCharacter].
	map at: aCharacter asciiValue + 1 put: 1.
	^aCharacter