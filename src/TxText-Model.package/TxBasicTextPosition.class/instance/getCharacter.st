accessing
getCharacter
	"Answer the character, right before the receiver,
	answer nil, if there's no character at given position"
	
	^ self span characterBeforePosition: self spanPosition ifNone: nil