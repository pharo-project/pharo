private composition
composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer
	theTextStyle := argTextStyle.
	theText := argText.
	theContainer := argContainer.
	self needComposition
		ifFalse: [ ^ self ].
	lines := argLinesCollection.
	deltaCharIndex := argDelta.
	currCharIndex := startCharIndex := argStart.
	stopCharIndex := argStop.
	prevLines := argPriorLines.
	currentY := argStartY.
	defaultLineHeight := 0. "(theTextStyle fontAt: theTextStyle defaultFontIndex) height" "+ theTextStyle leading".
	maxRightX := theContainer left.
	possibleSlide := stopCharIndex < theText size and: [ theContainer isMemberOf: Rectangle ].
	nowSliding := false.
	prevIndex := 1.
	scanner := RubCompositionScanner new text: theText textStyle: theTextStyle.
	isFirstLine := true.
	self composeAllLines.
	isFirstLine
		ifTrue: [ 
			"No space in container or empty text"
			self addNullLineWithIndex: startCharIndex andRectangle: (theContainer topLeft extent: theContainer width @ defaultLineHeight) ]
		ifFalse: [ self fixupLastLineIfCR ].
	prevLines := #()