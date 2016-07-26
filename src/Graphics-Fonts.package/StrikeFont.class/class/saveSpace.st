removing
saveSpace
	"Removes glyphs over 128, leaving only lower ascii.
	Also limit glyph depth to 4 bits (it is usually 16 or 32).
	This effectively turns off subpixel rendering, as glyphs will only have 16 shades of gray.
	
	StrikeFont saveSpace
	"
	StrikeFont allInstances do: [ :f | f
		stripHighGlyphs;
		setGlyphsDepthAtMost: 4 ].