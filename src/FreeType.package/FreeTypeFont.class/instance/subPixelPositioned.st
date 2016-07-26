testing
subPixelPositioned
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text."
	| settings |
	^subPixelPositioned 
		ifNil:[
			settings := FreeTypeSettings current.
			subPixelPositioned := settings hinting not or:[settings lightHinting]]