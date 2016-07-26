emphasis
makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right
		(may require more intercharacter space)"

	| g bonkForm bc font |
	1 to: fontArray size do: [:j |
		font := (fontArray at: j).
		font ifNotNil: [
			g := font glyphs deepCopy.
			"BonkForm will have bits where slanted characters overlap their neighbors."
			bonkForm := Form extent: (self height//4+2) @ self height.
			bc := font descent//4 + 1.  "Bonker x-coord corresponding to char boundary."
			bonkForm fill: (0 @ 0 corner: (bc+1) @ font ascent) fillColor: Color black.
			4 to: font ascent-1 by: 4 do:
				[:y | 		"Slide ascenders right..."
				g copy: (1@0 extent: g width @ (font ascent - y))
					from: 0@0 in: g rule: Form over.
				bonkForm copy: (1@0 extent: bonkForm width @ (font ascent - y))
					from: 0@0 in: bonkForm rule: Form over].
			bonkForm fill: (0 @ 0 corner: (bc+1) @ font ascent) fillColor: Color white.
			bonkForm fill: (bc @ font ascent corner: bonkForm extent) fillColor: Color black.
			font ascent to: font height-1 by: 4 do:
				[:y | 		"Slide descenders left..."
				g copy: (0@y extent: g width @ g height)
					from: 1@y in: g rule: Form over.
				bonkForm copy: (0@0 extent: bonkForm width @ bonkForm height)
					from: 1@0 in: bonkForm rule: Form over].
			bonkForm fill: (bc @ font ascent corner: bonkForm extent) fillColor: Color white.
			"Now use bonkForm to erase at every character boundary in glyphs."
			bonkForm offset: (0-bc) @ 0.
			font bonk: g with: bonkForm.
			font setGlyphs: g
		].
	].
