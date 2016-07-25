character shapes
makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph := self characterFormAt: Character space.
	glyph 
		border: glyph boundingBox
		width: 1
		fillColor: Color blue.
	self 
		characterFormAt: (Character value: 133)
		put: glyph.

	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(
		0
		1
		2
		3
		4
		5
		6
		7
		8
		11
		12
		14
		15
		16
		17
		18
		19
		20
		21
		22
		23
		24
		25
		26
		27
		28
		29
		30
		31
	) do: 
		[ :ascii | 
		characterToGlyphMap 
			at: ascii + 1
			put: 133 ]