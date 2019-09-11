"
I represent a paragraph of a text area
"
Class {
	#name : #RubParagraph,
	#superclass : #Object,
	#instVars : [
		'text',
		'composer',
		'container',
		'drawingEnabled',
		'textArea'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #querying }
RubParagraph class >> key [
	^ nil
]

{ #category : #private }
RubParagraph >> aboutToBeUnplugged [
]

{ #category : #editing }
RubParagraph >> actionAttributesUnder: aClickPoint event: anEvent do: aBlock [
	|startBlock|
	
	startBlock := self characterBlockAtPoint: aClickPoint.
	(self text attributesAt: startBlock stringIndex forStyle: self textStyle) 
		select: [ :attribute| attribute rubMayActOnEvent: anEvent ]
		thenDo: [:attribute | | range boxes| 
			"find the boxes for the current attribute range"
			range := self text rangeOf: attribute startingAt: startBlock stringIndex.
			boxes := self selectionRectsFrom: (self characterBlockForIndex: range first) 
						to: (self characterBlockForIndex: range last+1).
			boxes detect: [:each | each containsPoint: aClickPoint] ifFound: [ aBlock cull: attribute cull: boxes ]]
]

{ #category : #'accessing composer' }
RubParagraph >> actualWidth [
	^ self composer actualWidth
]

{ #category : #'geometry updating' }
RubParagraph >> adjustBottomY [
	| heights  bottomY verticalSize |
	heights := self lines collect: [ :each | each lineHeight ].
	verticalSize := heights sum.
	bottomY := self container top + verticalSize.
	self container: (self container withBottom: bottomY truncated).
]

{ #category : #'geometry updating' }
RubParagraph >> adjustRightX [
	| shrink |
	shrink := self container right - self maxRightX.
	self lines do: [ :line | line paddingWidth: (line paddingWidth - shrink)].
	self container: (self container withRight: self maxRightX)
]

{ #category : #querying }
RubParagraph >> characterBlockAtPoint: aPoint [ 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line := self lines at: (self lineIndexForPoint: aPoint).
	^ (RubCharacterBlockScanner new text: self text textStyle: self textStyle)
		characterBlockAtPoint: aPoint index: nil
		in: line
]

{ #category : #querying }
RubParagraph >> characterBlockForIndex: index [ 
	"Answer a CharacterBlock for the character in text at index."
	| line |
	line := self lines at: (self lineIndexOfCharacterIndex: index).
	^ (RubCharacterBlockScanner new text: self text textStyle: self textStyle)
		characterBlockAtPoint: nil index: ((index max: line first) min: self text size+1)
		in: line
]

{ #category : #editing }
RubParagraph >> click: anEvent for: model controller: editor [
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	
	| action clickPoint |
	
	clickPoint := anEvent cursorPoint.
	action := false.
	self actionAttributesUnder: clickPoint event: anEvent do: [ :attribute| |target|
		"evaluate the attribute action"
		target := (model ifNil: [textArea]).
		(attribute rubActOnClick: anEvent for: target in: self editor: editor) == true 	
			ifTrue: [ ^ true ]].
	
	(action and: [ Cursor currentCursor == Cursor webLink]) 
		ifTrue:[ Cursor normal show ].
	
	^ action
]

{ #category : #'accessing text area' }
RubParagraph >> closingDelimiters [
	^ textArea closingDelimiters

]

{ #category : #composition }
RubParagraph >> compose [
	self
		disableDrawingWhile: [ 
			self uptodateComposer 
				composeLinesFrom: 1
				to: self text size
				delta: 0
				into: OrderedCollection new
				priorLines: Array new
				atY: self compositionRectangle top.
			textArea ifNotNil: [ textArea paragraphWasComposedFrom: 1 to: self text size ] ]
]

{ #category : #accessing }
RubParagraph >> composer [
	^ composer ifNil: [ composer := self newComposer ]
]

{ #category : #composition }
RubParagraph >> compositionExtent [
	"Return the bounds for composing this text.  There are two cases:
	1.  wrapped is true -- grow downward as necessary,
	2.  wrapped is false -- grow in 2D as nexessary."

	^ (self wrapped
		ifTrue: [ self container width @ RubAbstractTextArea defaultMaxExtent ]
		ifFalse: [ RubAbstractTextArea defaultMaxExtent @ RubAbstractTextArea defaultMaxExtent ]) max: self minimumCompositionExtent
]

{ #category : #composition }
RubParagraph >> compositionRectangle [
	| e tl |
	e := self compositionExtent.
	tl := (self container insetBy: self margins) topLeft.
	^ tl corner: (tl x + e x - self margins right - (self margins left max: self cursorWidth)) @ e y
]

{ #category : #accessing }
RubParagraph >> container [
	^ container
]

{ #category : #accessing }
RubParagraph >> container: atextContainerOrRectangle [
	container := atextContainerOrRectangle.

]

{ #category : #querying }
RubParagraph >> containsPoint: aPoint [
	^ (self lines at: (self lineIndexForPoint: aPoint)) rectangle
		containsPoint: aPoint
]

{ #category : #copying }
RubParagraph >> copy [

	^ self error: 'A ParagraphHandler should never be copied'.
]

{ #category : #'accessing text area' }
RubParagraph >> cursorWidth [
	^ textArea cursorWidth

]

{ #category : #querying }
RubParagraph >> decoratorNamed: aKey [
	^ nil
]

{ #category : #copying }
RubParagraph >> deepCopy [
	"Don't want to copy the container (etc) or fonts in the TextStyle."

	^ self error: 'A ParagraphHandler should never be deep copied'.
]

{ #category : #querying }
RubParagraph >> defaultCharacterBlock [
	^ (RubCharacterBlock new
		stringIndex: 1
		topLeft: self lines first topLeft
		extent: 0 @ 0) textLine: self lines first
]

{ #category : #private }
RubParagraph >> defaultEmptyText [
	^ Text string: '' attributes: (Array with: self defaultFontChange)
]

{ #category : #private }
RubParagraph >> defaultFontChange [
	^ (TextFontChange fontNumber: self textStyle defaultFontIndex)
]

{ #category : #drawing }
RubParagraph >> disableDrawing [
	drawingEnabled := false
]

{ #category : #drawing }
RubParagraph >> disableDrawingWhile: aBlock [
	self drawingEnabled ifFalse: [ ^ aBlock value ].
	self disableDrawing.
	aBlock
		ensure: [ self enableDrawing ]
]

{ #category : #drawing }
RubParagraph >> displayTextOnAthensCanvas: anAthensCanvas in: aRectangle [
	| athensDisplayScanner offset leftInRun |
	anAthensCanvas clipBy: aRectangle during:[
	anAthensCanvas pathTransform
		restoreAfter: [ 
			offset := aRectangle topLeft - self position.
			athensDisplayScanner := AthensDisplayScanner for: self on: anAthensCanvas.
			leftInRun := 0.
			self
				forLinesIn: anAthensCanvas clipRect
				do: [ :line | 
					line first <= line last
						ifTrue: [ leftInRun := athensDisplayScanner displayLine: line offset: offset leftInRun: leftInRun ] ] ] ]
]

{ #category : #drawing }
RubParagraph >> drawOn: aCanvas using: aDisplayScanner at: aPosition [
	"Send all visible lines to the displayScanner for display"

	| offset leftInRun line visibleRectangle |
	self drawingEnabled
		ifFalse: [ ^ self ].
	visibleRectangle := aCanvas clipRect.
	offset := (aPosition - self position) truncated.
	leftInRun := 0.
	(self lineIndexForPoint: visibleRectangle topLeft) to: (self lineIndexForPoint: visibleRectangle bottomRight) do: [ :i | 
		line := self lines at: i.
		line first <= line last
			ifTrue: [ leftInRun := aDisplayScanner displayLine: line offset: offset leftInRun: leftInRun ] ]
]

{ #category : #drawing }
RubParagraph >> drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor [
	aCanvas setPaint: aColor.
	self displayTextOnAthensCanvas: aCanvas in: aRectangle
]

{ #category : #drawing }
RubParagraph >> drawingEnabled [
	^ drawingEnabled ifNil: [ drawingEnabled := true ]
]

{ #category : #drawing }
RubParagraph >> enableDrawing [
	drawingEnabled := true
]

{ #category : #geometry }
RubParagraph >> extent [
	self actualWidth ifNil: [ ^ self minimumExtent ].
	"lines empty check as workaround: lots of crashes on the CI, see github case 3879"
	self lines ifEmpty: [ ^ self minimumExtent ].
	^ (self actualWidth + self margins left + self margins right) @
			(self lines last bottom - self lines first top + self margins top + self margins bottom)
]

{ #category : #'geometry updating' }
RubParagraph >> extentFromClientBottomRight: aPoint [
	| w newExtent |
	self wrapped
		ifFalse: [ ^ self extent ].
	newExtent := aPoint.
	w := newExtent x max: self minimumExtent x.
	self container: (self container topLeft extent: (w @ newExtent y) truncated).
	self compose.
	^ newExtent x @ self extent y
]

{ #category : #'geometry updating' }
RubParagraph >> extentFromClientWidth: anInteger [
	^ self extentFromClientBottomRight: (anInteger @ self extent y)
]

{ #category : #'mock selection' }
RubParagraph >> findRegex [
	^ nil
]

{ #category : #querying }
RubParagraph >> firstLineCharacterIndexFromCharacterIndex: anIndex [
	^ (self lines at: (self lineIndexOfCharacterIndex: anIndex)) first
]

{ #category : #querying }
RubParagraph >> firstNonBlankCharacterBlockInLine: aLine [
	| idx str blanks |
	idx := aLine first.
	str := self text string.
	blanks := { Character space. Character tab }.
	[ idx <= aLine last ]
		whileTrue: [ | c |
			c := str at: idx.
			(blanks includes: c)
				ifFalse: [ ^ self characterBlockForIndex: idx ].
			idx := idx + 1 ].
	^ self characterBlockForIndex: aLine last 
]

{ #category : #drawing }
RubParagraph >> forLinesIn: aVisibleRect do: aBlock [
	(self lineIndexForPoint: aVisibleRect topLeft) to: 
	(self lineIndexForPoint: aVisibleRect bottomRight) do: 
		[:i | aBlock value: (self lines at: i) ]
]

{ #category : #composition }
RubParagraph >> forceCompose [
	self uptodateComposer prepareToForceComposition.
	self compose
]

{ #category : #querying }
RubParagraph >> hasDecorator: aDecorator [
	^ false
]

{ #category : #querying }
RubParagraph >> hasDecoratorNamed: aKey [
	^ false
]

{ #category : #querying }
RubParagraph >> indentationOfLineIndex: lineIndex ifBlank: aBlock [
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	^ self composer indentationOfLineIndex: lineIndex ifBlank: aBlock
]

{ #category : #initialization }
RubParagraph >> initialize [ 
	super initialize.
	drawingEnabled := true.

]

{ #category : #querying }
RubParagraph >> key [
	^self class key
]

{ #category : #querying }
RubParagraph >> lastLineCharacterIndexFromCharacterIndex: anIndex [
	| targetLine |
	targetLine := self lines at: (self lineIndexOfCharacterIndex: anIndex).
	^ targetLine = self lines last
		ifTrue: [ targetLine last + 1 ]
		ifFalse: [ targetLine last ]
]

{ #category : #querying }
RubParagraph >> lastNonBlankCharacterBlockInLine: aLine [
	| idx str blanks |
	idx := aLine last.
	str := self text string.
	blanks := { Character space. Character tab. Character cr. Character lf }.
	[ idx >= aLine first ]
		whileTrue: [ 
			(blanks includes: (str at: idx))
				ifFalse: [ ^ self characterBlockForIndex: idx ].
			idx := idx - 1 ].
	^ self characterBlockForIndex: aLine first 
]

{ #category : #querying }
RubParagraph >> lineIndexForPoint: aPoint [
	"Answer the index of the line in which to select the character nearest to aPoint."
	^ self composer lineIndexForPoint: aPoint
]

{ #category : #querying }
RubParagraph >> lineIndexOfCharacterIndex: characterIndex [ 
	"Answer the line index for a given characterIndex."
	"apparently the selector changed with NewParagraph"
	^ self composer lineIndexOfCharacterIndex: characterIndex

]

{ #category : #'accessing composer' }
RubParagraph >> lines [
	^ self composer lines
]

{ #category : #'accessing text area' }
RubParagraph >> margins [
	^textArea margins.

]

{ #category : #'accessing composer' }
RubParagraph >> maxRightX [
	^ self composer maxRightX
]

{ #category : #composition }
RubParagraph >> minimumCompositionExtent [

	^ self minimumExtent x - self margins left @ self minimumExtent y
]

{ #category : #geometry }
RubParagraph >> minimumExtent [
	^ textArea minimumExtent
]

{ #category : #'accessing text area' }
RubParagraph >> model [
	^ textArea model
]

{ #category : #editing }
RubParagraph >> move: anEvent for: model controller: editor [
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	
	| action clickPoint |
	
	clickPoint := anEvent cursorPoint.
	action := false.
	
	self actionAttributesUnder: clickPoint event: anEvent do: [ :attribute| |target|
		"evaluate the attribute action"
		target := (model ifNil: [textArea]).
		(attribute actOnMove: anEvent for: target in: self editor: editor) == true 	
			ifTrue: [ ^ true ]].
	
	(action and: [ Cursor currentCursor == Cursor webLink]) 
		ifTrue:[ Cursor normal show ].
	
	^ action
]

{ #category : #'geometry updating' }
RubParagraph >> moveBy: aPoint [
	container := container translateBy: aPoint.
	self composer moveBy: aPoint.
	textArea recomputeSelection.
	textArea invalidRect: (self position extent: self extent)

]

{ #category : #'accessing composer' }
RubParagraph >> newComposer [
	^ RubTextComposer new 
]

{ #category : #accessing }
RubParagraph >> next [ 
	^nil
]

{ #category : #'accessing composer' }
RubParagraph >> numberOfLines [

	^ self lines size
]

{ #category : #'accessing composer' }
RubParagraph >> numberOfPhysicalLines [

	^ self composer numberOfPhysicalLines
]

{ #category : #'accessing text area' }
RubParagraph >> openingDelimiters [
	^ textArea openingDelimiters

]

{ #category : #accessing }
RubParagraph >> paragraph [
	^ self
]

{ #category : #'accessing text area' }
RubParagraph >> pointIndex [
	^ textArea pointIndex
]

{ #category : #accessing }
RubParagraph >> position [
	^ container topLeft
]

{ #category : #composition }
RubParagraph >> recomposeFrom: start to: stop delta: delta [
	self
		disableDrawingWhile: [ 	self composer recomposeFrom: start to: stop delta: delta ]
]

{ #category : #private }
RubParagraph >> releaseComposer [
	composer
		ifNotNil: [ :c | 
			c unplug.
			composer := nil ]
]

{ #category : #editing }
RubParagraph >> replaceFrom: start to: stop with: aText [
	"Edit the text, and then recompose the lines."

	self
		disableDrawingWhile: [ 
			self composer emphasisHere: textArea emphasisHere.
			self composer replaceFrom: start to: stop with: aText.
			self text: self composer text.
			textArea paragraphWasComposedFrom: start to: stop.
			textArea paragraphReplacedTextFrom: start to: stop with: aText ]
]

{ #category : #editing }
RubParagraph >> replaceFrom: start to: stop with: aText displaying: displayBoolean [ 
	"Edit the text, and then recompose the lines." 
	self composer replaceFrom: start to: stop with: aText.

]

{ #category : #'accessing text area' }
RubParagraph >> scrollPane [
	^ textArea scrollPane
]

{ #category : #'mock selection' }
RubParagraph >> selection [
	^ self textArea selection
]

{ #category : #accessing }
RubParagraph >> selectionRects [
	"Return an array of rectangles representing the selection region."

	^ self selectionStart
		ifNil: [ Array new ]
		ifNotNil: [ self selectionRectsFrom: self selectionStart to: self selectionStop ]
]

{ #category : #accessing }
RubParagraph >> selectionRectsFrom: characterBlock1 to: characterBlock2 [
	"Return an array of rectangles representing the area between the two character blocks given as arguments."

	| line1 line2 cb1 cb2 |
	characterBlock1 = characterBlock2
		ifTrue: [ ^ #() ].
	characterBlock1 <= characterBlock2
		ifTrue: [ 
			cb1 := characterBlock1.
			cb2 := characterBlock2 ]
		ifFalse: [ 
			cb2 := characterBlock1.
			cb1 := characterBlock2 ].
	line1 := self lineIndexOfCharacterIndex: cb1 stringIndex.
	line2 := self lineIndexOfCharacterIndex: cb2 stringIndex.
	line1 = line2
		ifTrue: [ ^ Array with: (cb1 topLeft corner: cb2 bottomRight) ].
	^ Array
		streamContents: [ :strm | 
			| last |
			strm nextPut: (last := cb1 topLeft corner: (self lines at: line1) bottomRight).
			line1 + 1 to: line2 - 1 do: [ :i | 
				| line |
				line := self lines at: i.
				(line left = last left and: [ line right = last right ])
					ifTrue: [ 
						"new line has same margins as old one -- "
						"merge them, so that the caller gets as few rectangles as possible"
						last privateSetCorner: last right @ line bottom ]
					ifFalse: [ 
						"differing margins; cannot merge"
						strm nextPut: (last := line rectangle) ] ].
			strm nextPut: ((self lines at: line2) topLeft corner: cb2 bottomLeft) ]
]

{ #category : #'accessing text area' }
RubParagraph >> selectionStart [
	^ textArea selectionStart
]

{ #category : #'accessing text area' }
RubParagraph >> selectionStop [
	^ textArea selectionStop
]

{ #category : #accessing }
RubParagraph >> string [
	^ self text string
]

{ #category : #accessing }
RubParagraph >> tabWidth [
	^ self textStyle rubTabWidth
]

{ #category : #accessing }
RubParagraph >> tabWidth: anInteger [
	| newTextStyle tabsArray |
	newTextStyle := self textStyle copy.
	tabsArray := (anInteger to: 99999 by: anInteger) asArray.
	newTextStyle privateTabsArray: tabsArray.
	self textArea privateSetTextStyle: newTextStyle.
	self releaseComposer.
	self compose.
	self textArea recomputeSelection; changed
]

{ #category : #accessing }
RubParagraph >> text [
	^ text ifNil: [ text := self defaultEmptyText ]
]

{ #category : #accessing }
RubParagraph >> text: aText [
	(aText string notEmpty and: [aText runs isEmpty]) 
		ifTrue: [ aText runs: (Array with: self defaultFontChange) ].
	text := aText
]

{ #category : #accessing }
RubParagraph >> textArea [
	^ textArea
]

{ #category : #accessing }
RubParagraph >> textArea: aClient [
	"Classically, the client is a Morph which holds the text, the textstyle, the textColor.
	client must never be nil except when I'm released"
	textArea := aClient 
]

{ #category : #'accessing text area' }
RubParagraph >> textStyle [
	^ textArea textStyle
]

{ #category : #accessing }
RubParagraph >> theme [
	^ UITheme current
]

{ #category : #querying }
RubParagraph >> totalTextHeight [

	^self lines last bottom
]

{ #category : #initialization }
RubParagraph >> unplug [
	self releaseComposer.
	textArea := nil.
	text := nil.
	super unplug
]

{ #category : #'geometry updating' }
RubParagraph >> updateClientExtent [
	textArea updateExtentFromParagraph.

]

{ #category : #'accessing composer' }
RubParagraph >> uptodateComposer [
	^ self composer
		text: self text;
		textStyle: self textStyle;
		container: self compositionRectangle;
		emphasisHere: textArea emphasisHere;
		cursorWidth: textArea cursorWidth;
		yourself
]

{ #category : #querying }
RubParagraph >> verticesFrom: firstIndex to: lastIndex [
	| firstCB lastCB firstLineIndex lastLineIndex firstLine lastLine vertices secondLine thirdLine |
	firstCB := self characterBlockForIndex: firstIndex.
	lastCB := self characterBlockForIndex: lastIndex.
	firstLineIndex := self lineIndexOfCharacterIndex: firstIndex.
	lastLineIndex := self lineIndexOfCharacterIndex: lastIndex.
	firstLine := self lines at: firstLineIndex.
	lastLine := self lines at: lastLineIndex.
	vertices := OrderedCollection new.
	firstLine = lastLine
		ifTrue: [ 
			vertices add: firstCB bottomLeft.
			vertices add: firstCB topLeft.
			firstIndex ~= lastIndex
				ifTrue: [ 
					vertices add: lastCB topLeft.
					vertices add: lastCB bottomLeft.
					vertices add: firstCB bottomLeft ] ]
		ifFalse: [ 
			secondLine := self lines at: firstLineIndex + 1.
			thirdLine := self lines at: lastLineIndex - 1.
			vertices add: firstCB bottomLeft - (1@0).
			vertices add: firstCB topLeft - (1@0).
			vertices add: (self textArea right - self margins right) @ firstLine top.
			vertices add: (self textArea right - self margins right) @ thirdLine bottom.
			vertices add: lastCB topLeft.
			vertices add: lastCB bottomLeft.
			vertices add: lastLine bottomLeft - (1@0).
			vertices add: secondLine topLeft - (1@0).
			vertices add: firstCB bottomLeft - (1@0)].
	^ vertices
]

{ #category : #geometry }
RubParagraph >> width [
	^  self wrapped
		ifTrue: [ self container right  -  (self container insetBy: self margins) topLeft x]
		ifFalse: [ RubAbstractTextArea defaultMaxExtent ]
]

{ #category : #accessing }
RubParagraph >> withoutDecorator [
	^ self

]

{ #category : #'accessing text area' }
RubParagraph >> wrapped [
	^ textArea wrapped
]
