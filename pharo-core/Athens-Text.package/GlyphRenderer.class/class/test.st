as yet unclassified
test

	| fnt glyph |
	
	fnt := StandardFonts defaultFont.
	fnt class == LogicalFont ifTrue: [
		fnt := fnt realFont.
		].
	fnt class ~~ FreeTypeFont ifTrue: [ self error: 'invalid font' ].
	
	glyph := self  new initPixelSize: fnt pixelSize.
	
	glyph renderGlyph: $A asUnicode face: fnt face.
	glyph renderGlyph: $B asUnicode face: fnt face.
	glyph renderGlyph: $C asUnicode face: fnt face.
	glyph renderGlyph: $+ asUnicode face: fnt face.
	
	glyph drawOn: Display at: 0@0