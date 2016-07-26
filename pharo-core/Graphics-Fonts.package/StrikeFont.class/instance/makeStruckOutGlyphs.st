emphasis
makeStruckOutGlyphs
	"Make a struck-out set of glyphs with same widths"
	| g |
	g := glyphs deepCopy.
	g fillBlack: (0 @ (self ascent - (self ascent // 3)) extent: g width @ 1).
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 16 ]