drawing
drawLinesOn: aCanvas
	aCanvas
		line: self topLeft + (0 @ 25)
		to: self bottomLeft
		width: 1
		color: self borderColor.
	aCanvas
		line: self bottomLeft
		to: self bottomRight
		width: 1
		color: self borderColor.
	aCanvas
		line: self topRight + (0 @ 26)
		to: self bottomRight
		width: 1
		color: self borderColor