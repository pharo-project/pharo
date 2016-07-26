*FreeType-addition
isSubPixelPositioned
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text. 
	Currently, only FreeTypeFonts are subPixelPositioned, and only when not
	Hinted"
	
	^false 