computing
stepToFirstScanLineAt: yValue in: edgeTableEntry
	"Compute the initial x value for the scan line at yValue"
	|  startX endX startY endY yDir deltaY deltaX widthX |
	(start y) <= (end y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
		yDir := 1.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
		yDir := -1.
	].

	deltaY := endY - startY.
	deltaX := endX - startX.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[^edgeTableEntry lines: 0].

	"Check if edge goes left to right"
	deltaX >= 0 ifTrue:[
		xDirection := 1.
		widthX := deltaX.
		error := 0.
	] ifFalse:[
		xDirection := -1.
		widthX := 0 - deltaX.
		error := 1 - deltaY.
	].

	"Check if edge is horizontal"
	deltaY = 0 
		ifTrue:[	xIncrement := 0.
				errorAdjUp := 0]
		ifFalse:["Check if edge is y-major"
			deltaY > widthX 
				ifTrue:[	xIncrement := 0.
						errorAdjUp := widthX]
				ifFalse:[	xIncrement := (widthX // deltaY) * xDirection.
						errorAdjUp := widthX \\ deltaY]].

	errorAdjDown := deltaY.

	edgeTableEntry xValue: startX.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		startY to: yValue do:[:y| self stepToNextScanLineAt: y in: edgeTableEntry].
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].