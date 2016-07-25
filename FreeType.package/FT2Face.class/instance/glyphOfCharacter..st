glyphs
glyphOfCharacter: aCharacter
	"load a glyph with outline, glyph is not scaled "
	| em aGlyph |
	em := self unitsPerEm.
	self validate.
	self setPixelWidth: em height: em.
	self loadCharacter: aCharacter asInteger flags: LoadIgnoreTransform. "load glyph metrics"
	aGlyph := self glyph shallowCopy. " copy because 'face glyph' is only a slot"
	aGlyph outline: (self characterOutline: aCharacter).
	^aGlyph