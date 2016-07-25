building
buildFromForm: allGlyphs data: data name: aString 
	| x |
	pointSize := data first.
	ascent := data second.
	descent := data third.
	minAscii := 32.
	maxAscii := 255.
	name := aString.
	type := 0.	"ignored for now"
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	xTable := (Array new: 258) atAllPut: 0.
	maxWidth := 0.
	glyphs := allGlyphs.
	x := 0.
	minAscii 
		to: maxAscii + 1
		do: 
			[ :i | 
			x := data at: i - minAscii + 4.
			xTable 
				at: i + 1
				put: x ].
	xTable 
		at: 258
		put: x.
	self reset.
	derivativeFonts := Array new: 32