character shapes
extendMaxAsciiTo: newMax 
	"Extend the range of this font so that it can display glyphs up to newMax."
	newMax + 3 <= xTable size ifTrue: [ ^ self ].	"No need to extend."
	xTable size = (maxAscii + 3) ifFalse: [ ^ self error: 'This font is not well-formed.' ].

	"Insert a bunch of zero-width characters..."
	xTable := (xTable 
		copyFrom: 1
		to: maxAscii + 2) , ((maxAscii + 1 to: newMax) collect: [ :i | xTable at: maxAscii + 2 ]) , {  (xTable at: maxAscii + 3)  }.
	maxAscii := newMax.
	self fillZeroWidthSlots.
	characterToGlyphMap := nil