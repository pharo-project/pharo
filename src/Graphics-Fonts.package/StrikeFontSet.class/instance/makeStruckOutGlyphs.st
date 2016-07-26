emphasis
makeStruckOutGlyphs
	"Make a struck-out set of glyphs with same widths"

	| g font |
	1 to: fontArray size do: [:i |
		font := (fontArray at: i).
		font ifNotNil: [
			g := font glyphs deepCopy.
			g fillBlack: (0 @ (font ascent - (font ascent//3)) extent: g width @ 1).
			font setGlyphs: g
		].
	].
