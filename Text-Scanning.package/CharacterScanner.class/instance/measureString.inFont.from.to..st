scanning
measureString: aString inFont: aFont from: startIndex to: stopIndex
	"Measure aString width in given font aFont.
	The string shall not include line breaking, tab or other control character."
	destX := destY := lastIndex := 0.
	pendingKernX := 0.
	font := aFont.
	kern := 0 - font baseKern.
	spaceWidth := font widthOf: Space.
	stopConditions := MeasuringStopConditions.
	self scanCharactersFrom: startIndex to: stopIndex in: aString rightX: 999999.
	^destX