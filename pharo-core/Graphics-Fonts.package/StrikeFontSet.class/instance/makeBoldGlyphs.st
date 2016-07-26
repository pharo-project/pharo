emphasis
makeBoldGlyphs
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"

	| g bonkForm font |
	1 to: fontArray size do: [:i |
		font := fontArray at: i.
		font ifNotNil: [
			g := font glyphs deepCopy.
			bonkForm := (Form extent: 1@16) fillBlack offset: -1@0.
			self bonk: g with: bonkForm at: i.
			g copyBits: g boundingBox from: g at: (1@0)
				clippingBox: g boundingBox rule: Form under fillColor: nil.
			(fontArray at: i) setGlyphs: g.
		].
	].
