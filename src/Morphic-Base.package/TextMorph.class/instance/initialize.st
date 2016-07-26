initialization
initialize
	super initialize.
	borderWidth := 0.
	textStyle := TextStyle default copy.
	wrapFlag := true.
	margins := (0@0 corner: 0@0).
	
	self attachKeymapCategory: #TextMorph.