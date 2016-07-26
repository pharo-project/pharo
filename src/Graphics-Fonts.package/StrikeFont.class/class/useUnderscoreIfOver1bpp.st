character shapes
useUnderscoreIfOver1bpp
	"Sets underscore and caret glyphs for chars 95 and 94. 
	Only for enhanced StrikeFonts, i.e. those with glyphs of more than 1bpp.
	ASCII standard glyphs"
	"
	StrikeFont useUnderscoreIfOver1bpp
	"
	self allInstances do: [ :font | font useUnderscoreIfOver1bpp ]