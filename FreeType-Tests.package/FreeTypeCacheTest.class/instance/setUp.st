running
setUp

	cache := FreeTypeCache new.
	font1 := FreeTypeFont basicNew.
	font2 := FreeTypeFont basicNew.
	font3 := FreeTypeFont basicNew.
	font1XGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	font1YGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	font1ZGlyph := (GlyphForm extent: 100@100 depth: 32)
		advance: 100;
		linearAdvance: 10000;
		yourself.
	fullCache := FreeTypeCache new.
	fullCache 
		maximumSize: (10*(fullCache sizeOf: font1YGlyph))..
	1 to: 10 do:[:i |
		fullCache atFont: font1 charCode: i type: FreeTypeCacheGlyph put: font1YGlyph]. 