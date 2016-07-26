accessing
widthOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."
	^(self fontOf: aCharacter) widthOf: aCharacter