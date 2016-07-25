accessing
glyphOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."
	| code |
	(self hasGlyphOf: aCharacter) ifFalse: 
		[ fallbackFont ifNotNil: [ ^ fallbackFont glyphOf: aCharacter ].
		^ (Form extent: 1 @ self height) fillColor: Color white ].
	code := aCharacter charCode.
	^ glyphs copy: ((xTable at: code + 1) @ 0 corner: (xTable at: code + 2) @ self height)