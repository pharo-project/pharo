Class {
	#name : #RubEncryptedDisplayScanner,
	#superclass : #RubDisplayScanner,
	#category : #'Rubric-TextScanning'
}

{ #category : #scanning }
RubEncryptedDisplayScanner >> displayLine: textLine offset: offset leftInRun: leftInRun [
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."
	| stopCondition nowLeftInRun startIndex string lastPos |
	line := textLine.
	morphicOffset := offset.
	lineY := line top + offset y.
	lineHeight := line lineHeight.
	rightMargin := line rightMargin + offset x.
	lastIndex := line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin := (line leftMarginForAlignment: alignment) + offset x.
	destX := runX := leftMargin.
	fillBlt == nil ifFalse:
		["Not right"
		fillBlt destX: line left destY: lineY
			width: line width left height: lineHeight; copyBits].
	lastIndex := line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	baselineY := lineY + line baseline.
	destY := baselineY - font ascent.
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := String streamContents: [:s | 
		1 to: text string size do: [:i | s << '*']].
	[
		startIndex := lastIndex.
		lastPos := destX@destY.
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
						in: string rightX: rightMargin stopConditions: stopConditions
						kern: kern.
		lastIndex >= startIndex ifTrue:[
			bitBlt displayString: string 
				from: startIndex 
	"XXXX: The following is an interesting bug. All stopConditions exept #endOfRun
		have lastIndex past the last character displayed. #endOfRun sets it *on* the character.
		If we display up until lastIndex then we will also display invisible characters like
		CR and tab. This problem should be fixed in the scanner (i.e., position lastIndex
		consistently) but I don't want to deal with the fallout right now so we keep the
		fix minimally invasive."
				to: (stopCondition == #endOfRun ifTrue:[lastIndex] ifFalse:[lastIndex-1])
				at: lastPos kern: kern baselineY: baselineY font: font].
		"see setStopConditions for stopping conditions for displaying."
		self perform: stopCondition.
		"or: [lastIndex > runStopIndex]."
	] whileFalse.
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"
]
