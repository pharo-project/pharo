drawing
drawWeekDayNamesOn: aCanvas

	| cellHeight height topLeft topRight cellWidth |
	topLeft := self bounds topLeft translateBy: 0 @ 25.
	topRight := self bounds topRight translateBy: 0 @ 25.
	height := self bounds height - 25.
	cellHeight := height // 8.
	cellWidth := self bounds width // 7.
	aCanvas
		line: (topLeft translateBy: 0 @ cellHeight) 
		to: (topRight translateBy: 0 @ cellHeight) 
		width: 1
		color: Color black.

	#('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat') withIndexDo: [:dayName :dayIndex |
		| cellPosX cellTopCenter textWidth |
		cellPosX := cellWidth * (dayIndex - 1).
		cellTopCenter := topLeft translateBy: ((cellPosX + (cellWidth // 2)) + 1) @ 0.
		textWidth := self weekdayFont widthOfString: dayName.
		aCanvas
			drawString: dayName
			at: (cellTopCenter translateBy: (textWidth // -2) @ 3)
			font: self weekdayFont
			color: Color darkGray]
