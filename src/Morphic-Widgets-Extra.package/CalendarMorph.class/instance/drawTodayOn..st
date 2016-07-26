drawing
drawTodayOn: aCanvas

	| text textHeight textTopLeft textWidth textBox |
	text := 'Today: ', (Date today printFormat: #(2 1 3 $  3 1 1)).
	textWidth := self weekdayFont widthOfString: text.
	textHeight := self weekdayFont height.
	textTopLeft := self bounds bottomCenter translateBy: (textWidth // -2) @ (textHeight negated - 5).
	textBox := textTopLeft extent: textWidth @ textHeight.
	touchPoints at: textBox put: #handleTodayTouched.
	aCanvas
		drawString: text
		at: textTopLeft
		font: self weekdayFont
		color: Color gray