emphasis
makeUnderlinedGlyphs
	"Make an underlined set of glyphs with same widths"
	| g |
	g := glyphs deepCopy.
	g fillBlack: (0 @ (self ascent + 1) extent: g width @ 1).
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 4 ]