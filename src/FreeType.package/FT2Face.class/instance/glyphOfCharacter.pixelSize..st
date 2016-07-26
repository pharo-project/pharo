glyphs
glyphOfCharacter: aCharacter pixelSize: pixelSize
	"load a glyph with outline, glyph is not scaled "
	| aGlyph |
	self validate.
	self setPixelWidth: pixelSize x height: pixelSize y.
	self loadCharacter: aCharacter asUnicode flags: LoadIgnoreTransform. "load glyph metrics"
	aGlyph := self glyph. " copy because 'face glyph' is only a slot"
	glyph := nil.
	^aGlyph