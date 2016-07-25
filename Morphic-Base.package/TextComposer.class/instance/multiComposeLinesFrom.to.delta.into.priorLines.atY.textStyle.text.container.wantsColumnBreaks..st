protocol
multiComposeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer wantsColumnBreaks: argWantsColumnBreaks 
	wantsColumnBreaks := argWantsColumnBreaks.
	lines := argLinesCollection.
	theTextStyle := argTextStyle.
	theText := argText.
	theContainer := argContainer.
	deltaCharIndex := argDelta.
	currCharIndex := startCharIndex := argStart.
	stopCharIndex := argStop.
	prevLines := argPriorLines.
	currentY := argStartY.
	defaultLineHeight := theTextStyle lineGrid.
	maxRightX := theContainer left.
	possibleSlide := stopCharIndex < theText size
				and: [theContainer isMemberOf: Rectangle].
	nowSliding := false.
	prevIndex := 1.
	scanner := CompositionScanner new text: theText textStyle: theTextStyle.
	scanner wantsColumnBreaks: wantsColumnBreaks.
	isFirstLine := true.
	self composeAllLines.
	isFirstLine
		ifTrue: ["No space in container or empty text"
			self
				addNullLineWithIndex: startCharIndex
				andRectangle: (theContainer topLeft extent: 0 @ defaultLineHeight)]
		ifFalse: [self fixupLastLineIfCR].
	^ {lines asArray. maxRightX}