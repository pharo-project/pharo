scanning
displayLine: textLine offset: offset leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."
	| stopCondition nowLeftInRun startIndex string lastPos lineHeight stop |
	line := textLine.
	morphicOffset := offset.
	lineY := line top + offset y.
	lineHeight := line lineHeight.
	rightMargin := line rightMargin + offset x.
	lastIndex := line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin := (line leftMarginForAlignment: alignment) + offset x.
	destX := leftMargin.
	self fillTextBackground.
	lastDisplayableIndex := lastIndex := line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	destY := lineY + line baseline - font ascent.
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := text string.
	[
		"reset the stopping conditions of this displaying loop, and also the font."
		stopConditionsMustBeReset
			ifTrue:[self setStopConditions].
		
		"remember where this portion of the line starts"
		startIndex := lastIndex.
		lastPos := destX@destY.
		
		"find the end of this portion of the line"
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
						in: string rightX: rightMargin.
		"handle the stop condition - this will also set lastDisplayableIndex"
		stop := self perform: stopCondition.
		
		"display that portion of the line"
		lastDisplayableIndex >= startIndex ifTrue:[
			self displayString: string
				from: startIndex 
				to: lastDisplayableIndex 
				at: lastPos].
		
		"if the stop condition were true, stop the loop"
		stop
	] whileFalse.
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"