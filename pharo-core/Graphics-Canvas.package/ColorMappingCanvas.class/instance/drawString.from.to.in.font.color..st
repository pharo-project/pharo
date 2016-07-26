drawing-text
drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c
	"Draw the given string in the given font and color clipped to the given rectangle. If the font is nil, the default font is used."
	myCanvas
		drawString: s from: firstIndex to: lastIndex 
		in: boundsRect 
		font: fontOrNil 
		color: (self mapColor: c)