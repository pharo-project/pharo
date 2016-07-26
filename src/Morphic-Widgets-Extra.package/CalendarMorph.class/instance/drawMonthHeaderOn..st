drawing
drawMonthHeaderOn: aCanvas

	| headerWidth headerString box textBox textTopLeft monthBox monthNameWidth yearBox previousMonthBox nextMonthBox |
	headerString := date asMonth printString.
	headerWidth := self monthNameFont widthOfString: headerString.
	box := self bounds topLeft extent: self bounds width @ 23.
	textTopLeft := self bounds topCenter translateBy: (headerWidth // -2) @ 5.
	textBox := textTopLeft extent: headerWidth @ self monthNameFont height.
	monthNameWidth := self monthNameFont widthOfString: self date monthName, ' '.
	monthBox := textBox topLeft extent: monthNameWidth @ textBox height.
	yearBox := monthBox topRight corner: textBox bottomRight.
	previousMonthBox := (self bounds topLeft translateBy: 10 @ 5) extent: 10 @ self monthNameFont height.
	nextMonthBox := (self bounds topRight translateBy: -20 @ 5) extent: 10 @ self monthNameFont height.
	touchPoints
		at: monthBox put: #handleMonthNameTouched;
		at: yearBox put: #handleYearTouched;
		at: (previousMonthBox expandBy: 10 @ 5) put: #handlePreviousMonthTouched;
		at: (nextMonthBox expandBy: 10 @ 5) put: #handleNextMonthTouched.
	aCanvas
		frameAndFillRectangle: box
		fillColor: Color veryLightGray
		borderWidth: 1
		borderColor: Color black;
		
		line: box bottomLeft
		to: box bottomRight
		width: 2
		color: Color black;
		
		drawString: '<'
		at: previousMonthBox origin
		font: self monthNameFont
		color: Color black;
		
		drawString: '>'
		at: nextMonthBox origin
		font: self monthNameFont
		color: Color black;

		drawString: headerString
		at: (self bounds topCenter translateBy: (headerWidth // -2) @ 5)
		font: self monthNameFont
		color: Color black.
