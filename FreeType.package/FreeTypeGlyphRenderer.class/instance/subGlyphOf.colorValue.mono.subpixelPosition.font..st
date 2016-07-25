public
subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont
	"the default renderer does not support sub-pixel anti-aliasing,
	so answer an ordinary glyph"
	^self mode41GlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont
