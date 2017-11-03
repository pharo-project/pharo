Class {
	#name : #TextComposer,
	#superclass : #Object,
	#instVars : [
		'lines',
		'maxRightX',
		'currentY',
		'scanner',
		'possibleSlide',
		'nowSliding',
		'prevIndex',
		'prevLines',
		'currCharIndex',
		'startCharIndex',
		'stopCharIndex',
		'deltaCharIndex',
		'theText',
		'theContainer',
		'isFirstLine',
		'theTextStyle',
		'defaultLineHeight',
		'actualHeight',
		'wantsColumnBreaks'
	],
	#category : #'Morphic-Base-Text Support'
}

{ #category : #defaults }
TextComposer class >> characterForColumnBreak [

	^Character value: 12
]

{ #category : #adding }
TextComposer >> addNullLineForIndex: index [
"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	| oldLastLine r |

	oldLastLine := lines last.
	oldLastLine last - oldLastLine first >= 0 ifFalse: [^self].
	oldLastLine last = (index - 1) ifFalse: [^self].

	r := oldLastLine left @ oldLastLine bottom 
				extent: 0@(oldLastLine bottom - oldLastLine top).
	"Even though we may be below the bottom of the container,
	it is still necessary to compose the last line for consistency..."

	self addNullLineWithIndex: index andRectangle: r.

]

{ #category : #adding }
TextComposer >> addNullLineWithIndex: index andRectangle: r [

	lines addLast: (
		(
			TextLine 
				start: index 
				stop: index - 1
				internalSpaces: 0 
				paddingWidth: 0
		)
			rectangle: r;
			lineHeight: defaultLineHeight baseline: theTextStyle baseline
	)

]

{ #category : #testing }
TextComposer >> checkIfReadyToSlide [

	"Check whether we are now in sync with previously composed lines"

	(possibleSlide and: [currCharIndex > stopCharIndex]) ifFalse: [^self].

	[prevIndex < prevLines size
		and: [(prevLines at: prevIndex) first < (currCharIndex - deltaCharIndex)]]
			whileTrue: [prevIndex := prevIndex + 1].

	(prevLines at: prevIndex) first = (currCharIndex - deltaCharIndex) ifTrue: [
		"Yes -- next line will have same start as prior line."
		prevIndex := prevIndex - 1.
		possibleSlide := false.
		nowSliding := true
	] ifFalse: [
		prevIndex = prevLines size ifTrue: [
			"Weve reached the end of prevLines, so no use to keep looking for lines to slide."
			possibleSlide := false
		]
	]
]

{ #category : #composition }
TextComposer >> composeAllLines [

	[ currCharIndex <= theText size and: [ (currentY + defaultLineHeight) <= theContainer bottom ]] 
		whileTrue: [
			(nowSliding 
				ifTrue: [ self slideOneLineDown ] 
				ifFalse: [ self composeOneLine]) 
					ifNil: [ ^ nil ]].
]

{ #category : #composition }
TextComposer >> composeAllRectangles: rectangles [

	| charIndexBeforeLine numberOfLinesBefore reasonForStopping |

	actualHeight := defaultLineHeight.
	charIndexBeforeLine := currCharIndex.
	numberOfLinesBefore := lines size.
	reasonForStopping := self composeEachRectangleIn: rectangles.

	currentY := currentY + actualHeight.
	currentY > theContainer bottom ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex := charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [lines removeLast].
		^self
	].
	
	"It's OK -- the line still fits."
	maxRightX := maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline
	].
	isFirstLine := false.
	reasonForStopping == #columnBreak ifTrue: [^nil].
	currCharIndex > theText size ifTrue: [
		^nil		"we are finished composing"
	].
	
]

{ #category : #composition }
TextComposer >> composeEachRectangleIn: rectangles [

	| myLine lastChar |

	1 to: rectangles size do: [:i | 
		currCharIndex <= theText size ifFalse: [ ^ false ].
		myLine := scanner 
			composeFrom: currCharIndex 
			inRectangle: (rectangles at: i)				
			firstLine: isFirstLine 
			leftSide: i=1 
			rightSide: i=rectangles size.
		lines addLast: myLine.
		actualHeight := actualHeight max: myLine lineHeight.  "includes font changes"
		currCharIndex := myLine last + 1.
		
		"This happens if the text is just one line long"
		(myLine last = 0) ifTrue: [ ^ false ].
		
		lastChar := theText at: myLine last.
		(CharacterSet crlf includes: lastChar) ifTrue: [^#cr].
		wantsColumnBreaks ifTrue: [
			lastChar = self class characterForColumnBreak ifTrue: [^#columnBreak].
		].
	].
	^false
]

{ #category : #composition }
TextComposer >> composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer wantsColumnBreaks: argWantsColumnBreaks [ 
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
				andRectangle: (theContainer left @ theContainer top extent: 0 @ defaultLineHeight)]
		ifFalse: [self fixupLastLineIfCR].
	^ {lines asArray. maxRightX}
]

{ #category : #composition }
TextComposer >> composeOneLine [
	| rectangles |
	
	rectangles := theContainer 
					rectanglesAt: currentY 
					height: defaultLineHeight.
					
	rectangles notEmpty 
		ifTrue: [ (self composeAllRectangles: rectangles) 
					ifNil: [ ^ nil ]]
		ifFalse: [ currentY := currentY + defaultLineHeight ].
		
	self checkIfReadyToSlide
]

{ #category : #private }
TextComposer >> fixupLastLineIfCR [
"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	(theText size > 0 and: [ CharacterSet crlf includes: theText last ]) ifFalse: [ ^ self ].
	
	self addNullLineForIndex: theText size + 1.

]

{ #category : #protocol }
TextComposer >> multiComposeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer wantsColumnBreaks: argWantsColumnBreaks [ 
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
]

{ #category : #private }
TextComposer >> slideOneLineDown [

	| priorLine |

	"Having detected the end of rippling recoposition, we are only sliding old lines"
	prevIndex < prevLines size ifFalse: [
		"There are no more prevLines to slide."
		^nowSliding := possibleSlide := false
	].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex)
				slideIndexBy: deltaCharIndex andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1.
	wantsColumnBreaks ifTrue: [
		priorLine first to: priorLine last do: [ :i |
			(theText at: i) = TextComposer characterForColumnBreak ifTrue: [
				nowSliding := possibleSlide := false.
				^nil
			].
		].
	].

]
