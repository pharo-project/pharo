"
I represent the composer of a text, I am used to represent a paragraph
"
Class {
	#name : #RubTextComposer,
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
		'actualWidth',
		'numberOfPhysicalLines',
		'prevTextStyle',
		'prevText',
		'prevContainer',
		'emphasisHere',
		'cursorWidth'
	],
	#category : 'Rubric-Editing-Core'
}

{ #category : #accessing }
RubTextComposer >> actualWidth [
	^ actualWidth
]

{ #category : #adding }
RubTextComposer >> addNullLineForIndex: index [
	"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	| oldLastLine r |
	oldLastLine := lines last.
	oldLastLine last - oldLastLine first >= 0
		ifFalse: [ ^ self ].
	oldLastLine last = (index - 1)
		ifFalse: [ ^ self ].
	r := oldLastLine left @ oldLastLine bottom extent: oldLastLine right @ (oldLastLine bottom - oldLastLine top).	
	"Even though we may be below the bottom of the container,
	it is still necessary to compose the last line for consistency..."
	self addNullLineWithIndex: index andRectangle: r
]

{ #category : #adding }
RubTextComposer >> addNullLineWithIndex: index andRectangle: aRectangle [
	| nullString nullLine |
	nullString := ' ' asText.
	self emphasisHere do: [ :attr | nullString addAttribute: attr ].
	scanner text: nullString textStyle: theTextStyle.
	nullLine := scanner
		composeFrom: 1
		inRectangle: aRectangle
		firstLine: true
		leftSide: true
		rightSide: false.
	nullLine firstIndex: index lastIndex: index - 1.
	nullLine internalSpaces: 0 paddingWidth: 0.
	nullLine rectangle: ( aRectangle withBottom: nullLine bottom).
	lines addLast: nullLine 

]

{ #category : #testing }
RubTextComposer >> checkIfReadyToSlide [

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

{ #category : #'private composition' }
RubTextComposer >> completeComposition [
	numberOfPhysicalLines := 1.
	actualWidth := 1.
	self lines
		do: [ :line | 
			line lineNumber: numberOfPhysicalLines.
			line stopCondition = #cr
				ifTrue: [ numberOfPhysicalLines := numberOfPhysicalLines + 1 ].
			actualWidth := line actualWidth max: actualWidth ]
]

{ #category : #'private composition' }
RubTextComposer >> composeAllLines [
	[ currCharIndex <= theText size and: [ currentY + defaultLineHeight <= theContainer bottom ] ]
		whileTrue: [ 
			(nowSliding
				ifTrue: [ self slideOneLineDown ]
				ifFalse: [ self composeOneLine ]) ifNil: [ ^ nil ] ]
]

{ #category : #'private composition' }
RubTextComposer >> composeAllRectangles: rectangles [
	| charIndexBeforeLine numberOfLinesBefore |
	actualHeight := defaultLineHeight.
	charIndexBeforeLine := currCharIndex.
	numberOfLinesBefore := lines size.
	self composeEachRectangleIn: rectangles.
	currentY := currentY + actualHeight.
	currentY > theContainer bottom
		ifTrue: [ 
			"Oops -- the line is really too high to fit -- back out"
			currCharIndex := charIndexBeforeLine.
			lines size - numberOfLinesBefore timesRepeat: [ lines removeLast ].
			^ self ].	"It's OK -- the line still fits."
	maxRightX := maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i | 
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i) lineHeight: lines last lineHeight baseline: lines last baseline ].
	isFirstLine := false.
	currCharIndex > theText size
		ifTrue: [ ^ nil	"we are finished composing" ]
]

{ #category : #'private composition' }
RubTextComposer >> composeEachRectangleIn: rectangles [

	| myLine lastChar |

	1 to: rectangles size do: [:i | 
		currCharIndex <= theText size ifFalse: [ ^ false ].
		myLine := scanner 
			composeFrom: currCharIndex 
			inRectangle: (rectangles at: i)				
			firstLine: isFirstLine 
			leftSide: i=1 
			rightSide: i=rectangles size.
		myLine actualWidth: scanner rightX - theContainer left.
		
		lines addLast: myLine.
		actualHeight := actualHeight max: myLine lineHeight.  "includes font changes"
		currCharIndex := myLine last + 1.
		
		"This happens if the text is just one line long"
		(myLine last = 0) ifTrue: [ ^ false ].
		
		lastChar := theText at: myLine last.
		(CharacterSet crlf includes: lastChar) ifTrue: [^#cr].
	].
	^false
]

{ #category : #'public composition' }
RubTextComposer >> composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY [
	"While the section from start to stop has changed, composition may
	ripple all the way to the end of the text. However in a rectangular
	container, if we ever find a line beginning with the same character as
	before (ie corresponding to delta in the old lines), then we can just
	copy the old lines from there to the end of the container, with
	adjusted indices and y-values"

	self 
		composeLinesFrom: start
		to: stop
		delta: delta
		into: lineColl
		priorLines: priorLines
		atY: startingY
		textStyle: self textStyle
		text: self text
		container: theContainer.
	self completeComposition
]

{ #category : #'private composition' }
RubTextComposer >> composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer [
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
]

{ #category : #'private composition' }
RubTextComposer >> composeOneLine [
	| rectangles |
	rectangles := theContainer rectanglesAt: currentY height: defaultLineHeight.
	rectangles notEmpty
		ifTrue: [ (self composeAllRectangles: rectangles) ifNil: [ ^ nil ] ]
		ifFalse: [ currentY := currentY + defaultLineHeight ].
	self checkIfReadyToSlide
]

{ #category : #accessing }
RubTextComposer >> container [
	^ theContainer
]

{ #category : #accessing }
RubTextComposer >> container: aRectangle [ 
	theContainer := aRectangle
]

{ #category : #accessing }
RubTextComposer >> cursorWidth [
	^ cursorWidth
]

{ #category : #accessing }
RubTextComposer >> cursorWidth: anInteger [
	cursorWidth := anInteger
]

{ #category : #accessing }
RubTextComposer >> emphasisHere [
	^ emphasisHere ifNil: [ emphasisHere := ' ' asText attributesAt: 1 forStyle: self textStyle ]
]

{ #category : #accessing }
RubTextComposer >> emphasisHere: aListOfTextAttribute [
	emphasisHere := aListOfTextAttribute 
]

{ #category : #querying }
RubTextComposer >> fastFindFirstLineSuchThat: lineBlock [
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."
	| index low high |
	low := 1.
	high := self lines size.
	[index := high + low // 2.
	low > high]
		whileFalse: 
			[(lineBlock value: (self lines at: index))
				ifTrue: [high := index - 1]
				ifFalse: [low := index + 1]].
	^ low
]

{ #category : #private }
RubTextComposer >> fixupLastLineIfCR [
	"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	(theText size > 0 and: [ CharacterSet crlf includes: theText last ])
		ifFalse: [ ^ self ].
	self addNullLineForIndex: theText size + 1
]

{ #category : #querying }
RubTextComposer >> indentationOfLineIndex: lineIndex ifBlank: aBlock [
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last crlf |
	crlf := CharacterSet crlf.
	arrayIndex := lineIndex.
	[first := (self lines at: arrayIndex) first.
	 first > 1 and: [crlf includes: (self text string at: first - 1)]] whileTrue: "word wrap"
		[arrayIndex := arrayIndex - 1].
	last := (self lines at: arrayIndex) last.
	
	^(self text string copyFrom: first to: last) indentationIfBlank: aBlock.

]

{ #category : #querying }
RubTextComposer >> lineIndexForPoint: aPoint [
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py := aPoint y truncated.

	"Find the first line at this y-value"
	i := (self fastFindFirstLineSuchThat: [:line | line bottom > py]) min: self lines size.

	"Now find the first line at this x-value"
	[i < self lines size and: [(self lines at: i+1) top = (self lines at: i) top
				and: [aPoint x >= (self lines at: i+1) left]]]
		whileTrue: [i := i + 1].
	^ i
]

{ #category : #querying }
RubTextComposer >> lineIndexOfCharacterIndex: characterIndex [ 
	"Answer the line index for a given characterIndex."
	^ (self fastFindFirstLineSuchThat: [:line | line first > characterIndex]) - 1 max: 1
]

{ #category : #accessing }
RubTextComposer >> lines [
	^ lines
]

{ #category : #accessing }
RubTextComposer >> lines: aCollectionOfTextLine [
	lines := aCollectionOfTextLine
]

{ #category : #accessing }
RubTextComposer >> maxRightX [
	^ maxRightX
]

{ #category : #private }
RubTextComposer >> moveBy: aPoint [
	self lines do: [ :line | line moveBy: aPoint ].
	theContainer := theContainer translateBy: aPoint.
	maxRightX := maxRightX + aPoint x
]

{ #category : #'private composition' }
RubTextComposer >> needComposition [
	| ret |
	ret := prevText isNil.
	ret ifFalse: [ret := prevTextStyle ~= theTextStyle].
	ret ifFalse: [ret := prevContainer ~= theContainer].
	ret ifFalse: [ret := prevText string ~= theText string].
	ret ifFalse: [ret := prevText runs ~= theText runs].
	ret ifTrue: [
		prevText := theText copy.
		prevTextStyle := theTextStyle.
		prevContainer := theContainer].
	^ ret
]

{ #category : #accessing }
RubTextComposer >> numberOfPhysicalLines [
	^ numberOfPhysicalLines ifNil: [ numberOfPhysicalLines := 0 ]
]

{ #category : #'public composition' }
RubTextComposer >> prepareToForceComposition [
	prevText := nil
]

{ #category : #'private composition' }
RubTextComposer >> recomposeFrom: start to: stop delta: delta [
	"Recompose this paragraph. The altered portion is between start and
	stop. Recomposition may continue to the end of the text, due to a
	ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed."

	"Have to recompose line above in case a word-break was affected."

	| startLine newLines |
	self lines isEmptyOrNil ifTrue: [ ^self ].
	startLine := (self lineIndexOfCharacterIndex: start) - 1 max: 1.
	[ startLine > 1 and: [ (self lines at: startLine - 1) top = (self lines at: startLine) top ] ]
		whileTrue: [ startLine := startLine - 1 ].	"Find leftmost of line pieces"
	newLines := OrderedCollection new: self lines size + 1.
	1 to: startLine - 1 do: [ :i | newLines addLast: (self lines at: i) ].
	self
		composeLinesFrom: (self lines at: startLine) first
		to: stop
		delta: delta
		into: newLines
		priorLines: self lines
		atY: (self lines at: startLine) top.

]

{ #category : #'public composition' }
RubTextComposer >> replaceFrom: start to: stop with: aText [
	"Edit the text, and then recompose the lines."
	self text replaceFrom: start to: stop with: aText.
	self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop - start + 1).

]

{ #category : #private }
RubTextComposer >> slideOneLineDown [

	| priorLine |

	"Having detected the end of rippling recoposition, we are only sliding old lines"
	prevIndex < prevLines size ifFalse: [
		"There are no more prevLines to slide."
		^nowSliding := possibleSlide := false
	].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex) slideIndexBy: deltaCharIndex andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1
]

{ #category : #accessing }
RubTextComposer >> startCharIndex [
	^ startCharIndex
]

{ #category : #accessing }
RubTextComposer >> startCharIndex: anObject [
	^ startCharIndex := anObject
]

{ #category : #accessing }
RubTextComposer >> tabWidth [
	^ self textStyle rubTabWidth
]

{ #category : #accessing }
RubTextComposer >> text [
	^ theText 
	
]

{ #category : #accessing }
RubTextComposer >> text: aText [
	theText := aText
	
]

{ #category : #accessing }
RubTextComposer >> textStyle [
	^ theTextStyle 
]

{ #category : #accessing }
RubTextComposer >> textStyle: aTextStyle [ 
	theTextStyle := aTextStyle
]

{ #category : #private }
RubTextComposer >> unplug [
	theText := '' asText.
	lines := #().
	prevText := prevTextStyle := prevContainer := nil.
	super unplug
]
