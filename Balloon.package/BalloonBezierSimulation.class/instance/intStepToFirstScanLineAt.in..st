computing
intStepToFirstScanLineAt: yValue in: edgeTableEntry
	"Scaled integer version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	 scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[
		^edgeTableEntry lines: 0].

	fwX1 := (startX + endX - (2 * via x)).
	fwX2 := (via x - startX * 2).
	fwY1 := (startY + endY - (2 * via y)).
	fwY2 := ((via y - startY) * 2).
	maxSteps := deltaY asInteger * 2.
	scaledStepSize := 16r1000000 // maxSteps.
	"@@: Okay, we need some fancy 64bit multiplication here"
	squaredStepSize := self absoluteSquared8Dot24: scaledStepSize.
	squaredStepSize = ((scaledStepSize * scaledStepSize) bitShift: -24)
		ifFalse:[self error:'Bad computation'].
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx // 2).
	fwDy := fwDy + (fwDDy // 2).

	self validateIntegerRange.

	lastX := startX * 256.
	lastY := startY * 256.

	edgeTableEntry xValue: startX.
	edgeTableEntry yValue: startY.
	edgeTableEntry zValue: 0.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		self stepToNextScanLineAt: yValue in: edgeTableEntry.
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].