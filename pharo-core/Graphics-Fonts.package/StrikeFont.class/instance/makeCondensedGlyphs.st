emphasis
makeCondensedGlyphs
	"Make a condensed set of glyphs with same widths.
	NOTE: this has been superceded by kerning -- should not get called"
	| g newXTable x x1 w |
	g := glyphs deepCopy.
	newXTable := Array new: xTable size.
	newXTable 
		at: 1
		put: (x := xTable at: 1).
	1 
		to: xTable size - 1
		do: 
			[ :i | 
			x1 := xTable at: i.
			w := (xTable at: i + 1) - x1.
			w > 1 ifTrue: [ w := w - 1 ].	"Shrink every character wider than 1"
			g 
				copy: (x @ 0 extent: w @ g height)
				from: x1 @ 0
				in: glyphs
				rule: Form over.
			newXTable 
				at: i + 1
				put: (x := x + w) ].
	xTable := newXTable.
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont emphasized: 8 ]

	"
(TextStyle default fontAt: 1) copy makeCondensedGlyphs
	displayLine: 'The quick brown fox jumps over the lazy dog'
	at: Sensor cursorPoint
"