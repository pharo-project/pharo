drawing
drawWithSelectionOn: canvas
	canvas
		line: self bottomLeft + (0 @ -1)
		to: self selectedTab bottomLeft
		width: 1
		color: self borderColor.
	canvas
		line: self selectedTab bottomLeft
		to: self selectedTab  bottomRight
		width: 1
		color: self selectedColor.
	canvas
		line: self selectedTab  bottomRight
		to: self bottomRight + (0 @ -1)
		width: 1
		color: self borderColor