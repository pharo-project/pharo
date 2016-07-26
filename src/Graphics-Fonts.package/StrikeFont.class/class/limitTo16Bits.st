removing
limitTo16Bits
	"Limit glyph depth to 16 bits (it is usually 16 or 32).
	
	StrikeFont limitTo16Bits
	"
	StrikeFont allInstances do: [ :f | f
		setGlyphsDepthAtMost: 16 ].