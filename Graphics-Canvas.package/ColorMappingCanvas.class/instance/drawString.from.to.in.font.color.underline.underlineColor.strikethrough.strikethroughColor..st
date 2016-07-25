drawing-text
drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
	"Draw the given string in the given font and color clipped to the given rectangle. If the font is nil, the default font is used."
	myCanvas
		drawString: s 
		from: firstIndex 
		to: lastIndex 
		in: boundsRect 
		font: fontOrNil 
		color: (self mapColor: c) 
		underline: underline 
		underlineColor: (self mapColor: uc) 
		strikethrough: strikethrough 
		strikethroughColor: (self mapColor: sc)