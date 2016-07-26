emphasis
makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |
	extraOnLeft := (self height - 1 - self ascent + 4) // 4 max: 0.
	extraWidth := ((self ascent - 5 + 4) // 4 max: 0) + extraOnLeft.
	newGlyphs := Form 
		extent: (glyphs width + ((maxAscii + 1 - minAscii) * extraWidth)) @ glyphs height
		depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable := xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii 
		to: maxAscii + 1
		do: 
			[ :ascii | 
			x := xTable at: ascii + 1.
			w := (xTable at: ascii + 2) - x.
			newX := newXTable at: ascii + 1.
			newGlyphs 
				copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
				from: x @ 0
				in: glyphs
				rule: Form over.
			newXTable 
				at: ascii + 2
				put: newX + w + extraWidth ].
	glyphs := newGlyphs.
	xTable := newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 
		to: self ascent - 1
		by: 4
		do: 
			[ :y | 
			"Slide ascenders right..."
			glyphs 
				copy: (1 @ 0 extent: glyphs width @ (self ascent - y))
				from: 0 @ 0
				in: glyphs
				rule: Form over ].
	self ascent 
		to: self height - 1
		by: 4
		do: 
			[ :y | 
			"Slide descenders left..."
			glyphs 
				copy: (0 @ y extent: glyphs width @ glyphs height)
				from: 1 @ y
				in: glyphs
				rule: Form over ].
	self isSyntheticItalic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 2 ]