fonts
getFreetypeFontRendererFor: aFreeTypeFont 
	
	^ (FT2GlyphRenderer forFont: aFreeTypeFont surface: self)