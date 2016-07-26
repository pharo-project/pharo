drawing
drawMessageOn: aCanvas
	| factor rectangle width browseMessage |
	factor := self itemHeight.
	rectangle := self bounds top: self bounds bottom - factor.
	rectangle := rectangle left: self contentBounds left.
	aCanvas 
		line: rectangle topLeft 
		to: rectangle topRight
		color: Color darkGray.
	rectangle := rectangle top: rectangle top + 1.
	aCanvas
		drawString: '<- close detail'
		in: rectangle
		font: self messageFont
		color: Color darkGray.
	browseMessage := 'browse ->'.
	width := self messageFont widthOfString: browseMessage.
	aCanvas
		drawString: browseMessage
		in: (rectangle left: rectangle right - width)
		font: self messageFont
		color: Color darkGray