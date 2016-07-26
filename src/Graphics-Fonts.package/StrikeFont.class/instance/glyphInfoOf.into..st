accessing
glyphInfoOf: aCharacter into: glyphInfoArray
	"Answer the width of the argument as a character in the receiver."

	| code |
	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont glyphInfoOf: aCharacter into: glyphInfoArray.
		].
		code := 0.
	] ifTrue: [
		code := characterToGlyphMap 
				ifNotNil: [:map | map at: aCharacter charCode +1] 
				ifNil: [aCharacter charCode].
	].
	glyphInfoArray at: 1 put: glyphs;
		at: 2 put: (xTable at: code + 1);
		at: 3 put: (xTable at: code + 2);
		at: 4 put: (self ascentOf: aCharacter);
		at: 5 put: self.
	^ glyphInfoArray.
