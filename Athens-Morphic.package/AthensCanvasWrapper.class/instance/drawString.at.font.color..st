canvas drawing-text
drawString: aString at: aPoint font: aFontOrNil color: aColor
	self
		drawString: aString
		in: (origin + aPoint extent: self clipRect extent)
		font: aFontOrNil
		color: aColor