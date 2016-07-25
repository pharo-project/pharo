private-primitives
primGetCharIndex: characterCode
	"Return the glyph index of a given character code"
	<primitive: 'primitiveGetFaceCharIndex' module: 'FT2Plugin'>
	^self primitiveFailed.
