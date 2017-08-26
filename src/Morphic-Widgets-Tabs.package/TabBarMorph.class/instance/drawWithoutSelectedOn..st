drawing
drawWithoutSelectedOn: canvas
	canvas
		line: self bottomLeft + (0 @ -1)
		to: self bottomRight + (0 @ -1)
		width: 1
		color: self borderColor