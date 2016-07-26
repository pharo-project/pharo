building
stripHighGlyphs
	"Remove glyphs for characters above 128"
	| i |
	maxAscii := 127.
	xTable := xTable 
		copyFrom: 1
		to: maxAscii + 3.
	i := xTable at: maxAscii + 1.
	xTable 
		at: maxAscii + 2
		put: i.
	xTable 
		at: maxAscii + 3
		put: i.
	glyphs := glyphs copy: (0 @ 0 extent: i @ glyphs height).
	maxWidth := 0.
	2 
		to: xTable size
		do: [ :ii | maxWidth := maxWidth max: (xTable at: ii) - (xTable at: ii - 1) - 1 ].
	characterToGlyphMap := nil.
	self reset