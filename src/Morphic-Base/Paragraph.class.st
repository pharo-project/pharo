"
A Paragraph represents text that has been laid out, or composed, in some container.
I also display the different kinds of text selection (secondary, find-replace and selection bar).

Class collaborations
   SelectionBlock instances are built by myself and stored in the extraSelectionBlocks instance variable in order to display supplementary selections

Class main API
   no particular main API.

Instance Variables
   extraSelectionBlocks: <Collection of SelectionBlock>
   findReplaceSelectionRegex: <RxMatcher>
   presentationLines: <Object>
   presentationText: <Object>
   refreshExtraSelection: <Boolean>
   secondarySelection: <String>

extraSelectionBlocks
   - a collection of SelectionBlock for the drowing of the differents kind of text selection

findReplaceSelectionRegex
   - the find/replace matcher that is set from the editor of by the FindReplaceService

presentationLines
   - created for debugging purpose, should be removed ?

presentationText
   - created for debugging purpose, should be removed ?

refreshExtraSelection
   - a boolean that is set to tru when there is a need to refresh selections

secondarySelection
   - the string of the secondary selection that is set from the editor when a portion of text is selected

	text 		A Text with encoded per-character emphasis.
	textStyle	A TextStyle with font set, line height and horizontal alignment.
	firstCharacterIndex    The starting index in text for this paragraph, allowing
				composition of a long text into a number of containers.
	container	A Rectangle or TextContainer that determines where text can go.
	lines		An Array of TextLines comprising the final layout of the text
				after it has been composed within its container.
	positionWhenComposed   As its name implies.  Allows display at new locations
				without the need to recompose the text.
Lines are ordered vertically.  However, for a given y, there may be several lines in left to right order.  Lines must never be empty, even if text is empty.

Notes on yet another hack - 5 Feb 2001

We really need to clean up #composeLinesFrom:to:delta:into:priorLines:atY:!!!

I added one more habdful of code to correct:

This is an annoying bug that's been around for a couple of years, but I finally figured out how to duplicate the problem, so I figured I'd just report it now.  (It doesn't necessarily have to be fixed for 3.0 if it looks messy, but if it's a simple fix, it would be worth it.)

In Morphic, if you have the following text in a workspace:

This is line 1
This is line 2

**and** you have a return character after line 2, you will normally be able to click the mouse two times below line 2 in order to select all the text.  If you edit line 2 (e.g. so that it reads ""line number 2""), you can still select all the text by clicking below the second line.  However, if you edit line 1, you will not be able to select all the text from the bottom in the same way.  Things get messed up such that the last return character seems to be gone.  In this state, if you position the cursor immediately after the 2, and press the right arrow, the cursor jumps to the beginning of line 2... oof. (report by Doug Way)

While I don't have a very deep understanding of the above mentioned method, I was able to determine that text ending in a CR worked better in the editor when the last entry in <lines> had a start of text size + 1 and a stop of text size. I have accordingly added code near the end to ensure this. It seems to have fixed the problem, but we do need to clean this baby up some day. - Bob



"
Class {
	#name : #Paragraph,
	#superclass : #Object,
	#instVars : [
		'text',
		'textStyle',
		'firstCharacterIndex',
		'container',
		'lines',
		'positionWhenComposed',
		'offsetToEnd',
		'maxRightX',
		'selectionStart',
		'selectionStop',
		'wantsColumnBreaks',
		'focused',
		'caretRect',
		'showCaret',
		'findReplaceSelectionRegex',
		'secondarySelection',
		'extraSelectionBlocks',
		'refreshExtraSelection',
		'composer'
	],
	#classVars : [
		'InsertionPointColor'
	],
	#category : #'Morphic-Base-Text Support'
}

{ #category : #settings }
Paragraph class >> insertionPointColor [
	^ InsertionPointColor ifNil: [InsertionPointColor := (Color r: 0.0 g: 0.0 b: 0.8 alpha: 0.8)]
]

{ #category : #settings }
Paragraph class >> insertionPointColor: aColor [
	InsertionPointColor := aColor
]

{ #category : #editing }
Paragraph >> actionAttributesUnder: aClickPoint event: anEvent do: aBlock [
	| startBlock |
	startBlock := self characterBlockAtPoint: aClickPoint.
	(text attributesAt: startBlock stringIndex forStyle: textStyle)
		select: [ :attribute | attribute mayActOnEvent: anEvent ]
		thenDo: [ :attribute | 
			| range boxes |
			"find the boxes for the current attribute range"
			range := text rangeOf: attribute startingAt: startBlock stringIndex.
			boxes := self
				selectionRectsFrom: (self characterBlockForIndex: range first)
				to: (self characterBlockForIndex: range last + 1).
			boxes detect: [ :each | each containsPoint: aClickPoint ] ifFound: [ aBlock cull: attribute cull: boxes ] ]
]

{ #category : #private }
Paragraph >> adjustBottomY [
	| heights  bottomY verticalSize |
	heights := lines collect:[:each| each lineHeight ].
	verticalSize := heights sum.
	bottomY := container top + verticalSize.
	container := container withBottom: bottomY.
]

{ #category : #private }
Paragraph >> adjustLineIndicesBy: delta [
	firstCharacterIndex := firstCharacterIndex + delta.
	lines do: [:line | line slide: delta].

]

{ #category : #private }
Paragraph >> adjustRightX [
	| shrink |
	shrink := container right - maxRightX.
	lines do: [:line | line paddingWidth: (line paddingWidth - shrink)].
	container := container withRight: maxRightX
]

{ #category : #access }
Paragraph >> adjustedFirstCharacterIndex [
	"Return the index in the text where this paragraph WOULD begin if nothing had changed, except the size of the text -- ie if there have only been an insertion of deletion in the preceding morphs"
	offsetToEnd ifNil: [^ -1].
	^ text size - offsetToEnd
]

{ #category : #selection }
Paragraph >> buildSelectionBlocksFrom: topLeft to: bottomRight [
	| viewedString primary topLeftBlk bottomRightBlk findReplaceIntervals secondarySelectionIntervals startIdx stopIdx |
	Display depth = 1
		ifTrue: [ ^ self ].
	Display depth = 2
		ifTrue: [ ^ self ].
	primary := selectionStart notNil
		ifTrue: [ selectionStart stringIndex to: selectionStop stringIndex - 1 ]
		ifFalse: [ 0 to: -1 ].
	topLeftBlk := self characterBlockAtPoint: topLeft.
	bottomRightBlk := self characterBlockAtPoint: bottomRight.
	startIdx := topLeftBlk stringIndex.
	stopIdx := bottomRightBlk stringIndex.
	viewedString := (self text copyFrom: startIdx to: stopIdx) asString.
	self theme currentSettings haveSecondarySelectionTextColor
		ifTrue: [ self text removeAttribute: TextSelectionColor secondarySelection ].
	self theme currentSettings haveFindReplaceSelectionTextColor
		ifTrue: [ self text removeAttribute: TextSelectionColor findReplaceSelection ].
	findReplaceIntervals := #().
	extraSelectionBlocks := Array
		streamContents: [ :strm | 
			findReplaceSelectionRegex
				ifNotNil: [ 
					findReplaceIntervals := findReplaceSelectionRegex matchingRangesIn: viewedString.
					findReplaceIntervals := (findReplaceIntervals collect: [ :r | r + topLeftBlk stringIndex - 1 ])
						reject: [ :r | primary size > 0 and: [ (r includes: primary first) or: [ primary includes: r first ] ] ].
					findReplaceIntervals
						do: [ :r | 
							self theme currentSettings haveFindReplaceSelectionTextColor
								ifTrue: [ self text addAttribute: TextSelectionColor findReplaceSelection from: r first to: r last ].
							strm
								nextPut:
									(ParagraphSelectionBlock
										first: (self characterBlockForIndex: r first)
										last: (self characterBlockForIndex: r last + 1)
										color: self findReplaceSelectionColor) ] ].
			secondarySelection
				ifNotNil: [ 
					secondarySelectionIntervals := (secondarySelection
						reject: [ :i | (findReplaceIntervals includes: i) or: [ i = primary or: [ i first > self text size ] ] ])
						collect: [ :i | i first to: (i last min: self text size) ].
					secondarySelectionIntervals
						do: [ :r | 
							self theme currentSettings haveSecondarySelectionTextColor
								ifTrue: [ self text addAttribute: TextSelectionColor secondarySelection from: r first to: r last ].
							strm
								nextPut:
									(ParagraphSelectionBlock
										first: (self characterBlockForIndex: r first)
										last: (self characterBlockForIndex: r last + 1)
										color: self secondarySelectionColor) ] ] ].
	findReplaceSelectionRegex := nil.
	secondarySelection := nil
]

{ #category : #access }
Paragraph >> caretRect [
	"The rectangle in which the caret was last drawn,
	 or nil if the last drawing drew a range-selection rather than insertion point."
	^ caretRect
]

{ #category : #access }
Paragraph >> caretWidth [
	^ TextEditor dumbbellCursor		
			ifTrue: [ 2 ]		
			ifFalse: [ 0 ]
]

{ #category : #alignment }
Paragraph >> centered [ 
	textStyle centered
]

{ #category : #selection }
Paragraph >> characterBlockAtPoint: aPoint [ 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line := lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: text textStyle: textStyle)
		characterBlockAtPoint: aPoint index: nil
		in: line
]

{ #category : #selection }
Paragraph >> characterBlockForIndex: index [ 
	"Answer a CharacterBlock for the character in text at index."
	| line |
	line := lines at: (self lineIndexForCharacter: index).
	^ (CharacterBlockScanner new text: text textStyle: textStyle)
		characterBlockAtPoint: nil index: ((index max: line first) min: text size+1)
		in: line
]

{ #category : #editing }
Paragraph >> click: anEvent for: model controller: editor [
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	
	| action clickPoint |
	
	clickPoint := anEvent cursorPoint.
	action := false.
	self actionAttributesUnder: clickPoint event: anEvent do: [ :attribute| |target|
		"evaluate the attribute action"
		target := (model ifNil: [editor morph]).
		(attribute actOnClick: anEvent for: target in: self editor: editor) == true 	
			ifTrue: [ ^ true ]].
	
	(action and: [ Cursor currentCursor == Cursor webLink]) 
		ifTrue:[ Cursor normal show ].
	
	^ action
]

{ #category : #composition }
Paragraph >> compose: t style: ts from: startingIndex in: textContainer [
	text := t.
	textStyle := ts.
	firstCharacterIndex := startingIndex.
	offsetToEnd := text size - firstCharacterIndex.
	container := textContainer.
	self composeAll
]

{ #category : #composition }
Paragraph >> composeAll [
	^ self
		multiComposeLinesFrom: firstCharacterIndex
		to: text size
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: container top 
]

{ #category : #composition }
Paragraph >> composeAllStartingAt: characterIndex [
	firstCharacterIndex := characterIndex.
	offsetToEnd := text size - firstCharacterIndex.
	self composeAll
]

{ #category : #composition }
Paragraph >> composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines
	atY: startingY [
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult |

	newResult := self composer
		composeLinesFrom: start 
		to: stop 
		delta: delta 
		into: lineColl 
		priorLines: priorLines
		atY: startingY
		textStyle: textStyle 
		text: text 
		container: container
		wantsColumnBreaks: wantsColumnBreaks == true.
	lines := newResult first asArray.
	^ maxRightX := newResult second.

]

{ #category : #access }
Paragraph >> composer [
	^ composer ifNil: [composer := TextComposer new]
]

{ #category : #composition }
Paragraph >> compositionRectangle [
	^ container
]

{ #category : #selection }
Paragraph >> containsPoint: aPoint [
	^ (lines at: (self lineIndexForPoint: aPoint)) rectangle
		containsPoint: aPoint
]

{ #category : #copying }
Paragraph >> deepCopy [
	"Don't want to copy the container (etc) or fonts in the TextStyle."
	| new |
	new := self copy.
	new textStyle: textStyle copy
		lines: lines copy
		text: text deepCopy.
	^ new
]

{ #category : #selection }
Paragraph >> defaultCharacterBlock [
	^ (CharacterBlock new stringIndex: firstCharacterIndex text: text
			topLeft: lines first topLeft extent: 0 @ 0)
		textLine: lines first
]

{ #category : #display }
Paragraph >> displayExtraSelectionOn: aCanvas [ 
	"Send all visible lines to the displayScanner for display"

	| visibleRectangle line |
	visibleRectangle := aCanvas clipRect.
	refreshExtraSelection = true 
		ifTrue: [self buildSelectionBlocksFrom: visibleRectangle topLeft to: visibleRectangle bottomRight.
			refreshExtraSelection := false].
	extraSelectionBlocks ifNotNil: [
		(self lineIndexForPoint: visibleRectangle topLeft)
			to: (self lineIndexForPoint: visibleRectangle bottomRight)
			do: [:i | line := lines at: i.
				extraSelectionBlocks
					do: [:selblock | self displaySelectionBlock: selblock inLine: line on: aCanvas]]]
]

{ #category : #display }
Paragraph >> displayOn: aCanvas using: displayScanner at: somePosition [
	"Send all visible lines to the displayScanner for display"
	| visibleRectangle offset leftInRun line |
	visibleRectangle := aCanvas clipRect.
	offset := (somePosition - positionWhenComposed) truncated.
	leftInRun := 0.
	(self lineIndexForPoint: visibleRectangle topLeft)
		to: (self lineIndexForPoint: visibleRectangle bottomRight)
		do: [:i | line := lines at: i.
			self displaySelectionInLine: line on: aCanvas.
			line first <= line last ifTrue:
				[leftInRun := displayScanner displayLine: line offset: offset leftInRun: leftInRun]].

]

{ #category : #display }
Paragraph >> displaySelectionBarOn: aCanvas [ 
	| visibleRectangle line |
	selectionStart ifNil: [^ self].
	selectionStop ifNil: [^ self].
	visibleRectangle := aCanvas clipRect.
	selectionStart textLine = selectionStop textLine ifFalse: [^self].
	line := selectionStart textLine.
	aCanvas fillRectangle: (visibleRectangle left @ line top corner: visibleRectangle right @ line bottom) color: self selectionBarColor
]

{ #category : #display }
Paragraph >> displaySelectionBlock: aSelBlock inLine: line on: aCanvas [ 
	"Display a SelectionBlock if it does not overlap vith the regular selection"
	
	(selectionStart notNil and: [selectionStop notNil and: [selectionStart ~= selectionStop]])
		ifTrue: [	| startIdx stopIdx selSartIdx selStopIdx selBlockRange selRange |
			startIdx := aSelBlock first stringIndex.
			stopIdx := aSelBlock last stringIndex.
			selSartIdx := selectionStart stringIndex.
			selStopIdx := selectionStop stringIndex.
			selBlockRange := (startIdx) to: (stopIdx).
			selRange := (selSartIdx) to: (selStopIdx).
			((selBlockRange rangeIncludes: selSartIdx + 1) or: [(selBlockRange rangeIncludes: selStopIdx - 1) 
				or: [(selRange rangeIncludes: startIdx + 1) or: [selRange rangeIncludes: stopIdx - 1]]])
				ifTrue: [^ self]]. "regular selection and this selection block overlap"
	aSelBlock displayInLine: line on: aCanvas

]

{ #category : #display }
Paragraph >> displaySelectionInLine: line on: aCanvas [ 
	| leftX rightX w caretColor |
	selectionStart ifNil: [^self].	"No selection"
	aCanvas isShadowDrawing ifTrue: [ ^self ].	"don't draw selection with shadow"
	selectionStart = selectionStop 
		ifTrue: 
			["Only show caret on line where clicked"

			selectionStart textLine ~= line ifTrue: [^self]]
		ifFalse: 
			["Test entire selection before or after here"

			(selectionStop stringIndex < line first 
				or: [selectionStart stringIndex > (line last + 1)]) ifTrue: [^self].	"No selection on this line"
			(selectionStop stringIndex = line first 
				and: [selectionStop textLine ~= line]) ifTrue: [^self].	"Selection ends on line above"
			(selectionStart stringIndex = (line last + 1) 
				and: [selectionStop textLine ~= line]) ifTrue: [^self]].	"Selection begins on line below"
	leftX := (selectionStart stringIndex < line first 
				ifTrue: [line ]
				ifFalse: [selectionStart ])left.
	rightX := (selectionStop stringIndex > (line last + 1) or: 
					[selectionStop stringIndex = (line last + 1) 
						and: [selectionStop textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [selectionStop left].
	selectionStart = selectionStop 
		ifTrue: 
			[rightX := rightX + 1.
			w := self caretWidth.
			caretRect := (leftX-w) @ line top corner: (rightX+w)@ line bottom.
			self showCaret ifFalse:[^self].
			caretColor := self insertionPointColor.
			1 to: w
				do: 
					[:i | 
					"Draw caret triangles at top and bottom"

					aCanvas fillRectangle: ((leftX - w + i - 1) @ (line top + i - 1) 
								extent: ((w - i) * 2 + 3) @ 1)
						color: caretColor.
					aCanvas fillRectangle: ((leftX - w + i - 1) @ (line bottom - i) 
								extent: ((w - i) * 2 + 3) @ 1)
						color: caretColor].
			aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom)
				color: caretColor]
		ifFalse: 
			[caretRect := nil.
			aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom)
				color: self selectionColor]
]

{ #category : #access }
Paragraph >> extent [
	^ container width @ (lines last bottom - lines first top)
]

{ #category : #selection }
Paragraph >> extraSelectionChanged [
	refreshExtraSelection := true
]

{ #category : #selection }
Paragraph >> extraSelectionRects [
	"Return an array of rectangles representing the findReplace and the secondary selection regions."
	^ Array streamContents: [:strm |
		extraSelectionBlocks 
			ifNotNil: [:blocks | 
				blocks do: [:selBlock | strm nextPutAll: (self selectionRectsFrom: selBlock first to: selBlock last)]]]

]

{ #category : #private }
Paragraph >> fastFindFirstLineSuchThat: lineBlock [
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."
	| index low high |
	low := 1.
	high := lines size.
	[index := high + low // 2.
	low > high]
		whileFalse: 
			[(lineBlock value: (lines at: index))
				ifTrue: [high := index - 1]
				ifFalse: [low := index + 1]].
	^ low
]

{ #category : #selection }
Paragraph >> findReplaceSelectionColor [
	^  self theme currentSettings findReplaceSelectionColor

]

{ #category : #selection }
Paragraph >> findReplaceSelectionRegex: aRegex [
	findReplaceSelectionRegex := aRegex.

]

{ #category : #access }
Paragraph >> firstCharacterIndex [
	^ firstCharacterIndex
]

{ #category : #access }
Paragraph >> focused [
	^ focused ifNil: [focused := false]

]

{ #category : #access }
Paragraph >> focused: aBoolean [
	focused := aBoolean
]

{ #category : #selection }
Paragraph >> hasExtraSelection [
	"Return true if I've some findReplace or secondary selection"
	^ extraSelectionBlocks isEmptyOrNil not

]

{ #category : #private }
Paragraph >> indentationOfLineIndex: lineIndex ifBlank: aBlock [
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last crlf |
	crlf := CharacterSet crlf.
	arrayIndex := lineIndex.
	[first := (lines at: arrayIndex) first.
	 first > 1 and: [crlf includes: (text string at: first - 1)]] whileTrue: "word wrap"
		[arrayIndex := arrayIndex - 1].
	last := (lines at: arrayIndex) last.
	
	^(text string copyFrom: first to: last) indentationIfBlank: aBlock.

]

{ #category : #initialization }
Paragraph >> initialize [
	super initialize.
	self positionWhenComposed: 0 @ 0
]

{ #category : #settings }
Paragraph >> insertionPointColor [
	self focused ifFalse: [^ Color transparent].
	^ Display depth <= 2
		ifTrue: [Color black]
		ifFalse: [self class insertionPointColor]
]

{ #category : #alignment }
Paragraph >> justified [ 
	textStyle justified
]

{ #category : #access }
Paragraph >> lastCharacterIndex [
	^ lines last last
]

{ #category : #alignment }
Paragraph >> leftFlush [ 
	textStyle leftFlush
]

{ #category : #private }
Paragraph >> lineIndexForCharacter: index [
	"Answer the index of the line in which to select the character at index."
	^ (self fastFindFirstLineSuchThat: [:line | line first > index]) - 1 max: 1
]

{ #category : #private }
Paragraph >> lineIndexForPoint: aPoint [
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py := aPoint y truncated.

	"Find the first line at this y-value"
	i := (self fastFindFirstLineSuchThat: [:line | line bottom > py]) min: lines size.

	"Now find the first line at this x-value"
	[i < lines size and: [(lines at: i+1) top = (lines at: i) top
				and: [aPoint x >= (lines at: i+1) left]]]
		whileTrue: [i := i + 1].
	^ i
]

{ #category : #private }
Paragraph >> lineIndexOfCharacterIndex: characterIndex [ 
	"Answer the line index for a given characterIndex."
	"apparently the selector changed with NewParagraph"

	^self lineIndexForCharacter: characterIndex 

]

{ #category : #private }
Paragraph >> lines [
	^ lines ifNil: [ ^ Array new ]
]

{ #category : #access }
Paragraph >> maxRightX [
	^ maxRightX
]

{ #category : #editing }
Paragraph >> move: anEvent for: model controller: editor [
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	
	| action clickPoint |
	
	clickPoint := anEvent cursorPoint.
	action := false.
	
	self actionAttributesUnder: clickPoint event: anEvent do: [ :attribute| |target|
		"evaluate the attribute action"
		target := (model ifNil: [editor morph]).
		(attribute actOnMove: anEvent for: target in: self editor: editor) == true 	
			ifTrue: [ ^ true ]].
	
	(action and: [ Cursor currentCursor == Cursor webLink]) 
		ifTrue:[ Cursor normal show ].
	
	^ action
]

{ #category : #private }
Paragraph >> moveBy: delta [
	lines do: [:line | line moveBy: delta].
	positionWhenComposed ifNotNil:[
	positionWhenComposed := positionWhenComposed + delta].
	container := container translateBy: delta
]

{ #category : #composition }
Paragraph >> multiComposeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY [ 
	"While the section from start to stop has changed, composition may
	ripple all the way to the end of the text. However in a rectangular
	container, if we ever find a line beginning with the same character as
	before (ie corresponding to delta in the old lines), then we can just
	copy the old lines from there to the end of the container, with
	adjusted indices and y-values"
	| newResult |
	newResult := self composer
				multiComposeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				textStyle: textStyle
				text: text
				container: container
				wantsColumnBreaks: wantsColumnBreaks == true.
	lines := newResult first asArray.
	"maxRightX printString displayAt: 0@0."
	^ maxRightX := newResult second.


]

{ #category : #access }
Paragraph >> numberOfLines [

	^lines size
]

{ #category : #private }
Paragraph >> positionWhenComposed: pos [
	positionWhenComposed := pos
]

{ #category : #composition }
Paragraph >> recomposeFrom: start to: stop delta: delta [ 
	"Recompose this paragraph. The altered portion is between start and
	stop. Recomposition may continue to the end of the text, due to a
	ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed."
	"Have to recompose line above in case a word-break was affected."
	| startLine newLines |
	startLine := (self lineIndexForCharacter: start)
				- 1 max: 1.
	[startLine > 1
		and: [(lines at: startLine - 1) top = (lines at: startLine) top]]
		whileTrue: [startLine := startLine - 1].
	"Find leftmost of line pieces"
	newLines := OrderedCollection new: lines size + 1.
	1
		to: startLine - 1
		do: [:i | newLines
				addLast: (lines at: i)].
	text string isOctetString ifTrue: [
		^ self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top.
	].
	self
		multiComposeLinesFrom: (lines at: startLine) first
		to: stop
		delta: delta
		into: newLines
		priorLines: lines
		atY: (lines at: startLine) top
]

{ #category : #editing }
Paragraph >> replaceFrom: start to: stop with: aText [
	"Edit the text, and then recompose the lines." 
	text replaceFrom: start to: stop with: aText.
	self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop-start+1)
]

{ #category : #editing }
Paragraph >> replaceFrom: start to: stop with: aText displaying: displayBoolean [ 
	"Edit the text, and then recompose the lines." 
	text replaceFrom: start to: stop with: aText.
	self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop-start+1)
]

{ #category : #alignment }
Paragraph >> rightFlush [ 
	textStyle rightFlush
]

{ #category : #access }
Paragraph >> secondarySelection [
	^ secondarySelection
]

{ #category : #selection }
Paragraph >> secondarySelection: aSubstring [
	secondarySelection := aSubstring.

]

{ #category : #selection }
Paragraph >> secondarySelectionColor [
	^  self theme secondarySelectionColor

]

{ #category : #selection }
Paragraph >> selectionBarColor [
	^ self theme  selectionBarColor
]

{ #category : #selection }
Paragraph >> selectionColor [
	^ self focused 
		ifTrue: [ self theme selectionColor]
		ifFalse: [ self theme unfocusedSelectionColor ].

]

{ #category : #selection }
Paragraph >> selectionColor: aColor [ 
	"ignored"
]

{ #category : #selection }
Paragraph >> selectionContainsPoint: aPoint [
	"return whether the current selection contains the given point"
	^ self selectionRects anySatisfy: [ :rect| rect containsPoint: aPoint ]
]

{ #category : #selection }
Paragraph >> selectionRects [
	"Return an array of rectangles representing the selection region."
	selectionStart ifNil: [^ Array new].
	^ self selectionRectsFrom: selectionStart to: selectionStop
]

{ #category : #selection }
Paragraph >> selectionRectsFrom: characterBlock1 to: characterBlock2 [ 
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 := characterBlock1.  cb2 := characterBlock2]
		ifFalse: [cb2 := characterBlock1.  cb1 := characterBlock2].
	cb1 = cb2 ifTrue:
		[w := self caretWidth.
		^ Array with: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 := self lineIndexForCharacter: cb1 stringIndex.
	line2 := self lineIndexForCharacter: cb2 stringIndex.
	line1 = line2 ifTrue:
		[^ Array with: (cb1 topLeft corner: cb2 bottomRight)].
	rects := OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		| line |
		line := lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect := rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].

	rects addLast: ((lines at: line2) topLeft rectangle: cb2 bottomLeft).
	^ rects
]

{ #category : #selection }
Paragraph >> selectionStart: startBlock selectionStop: stopBlock [
	selectionStart := startBlock.
	selectionStop := stopBlock.
]

{ #category : #access }
Paragraph >> showCaret [
	^showCaret ifNil:[true]

]

{ #category : #access }
Paragraph >> showCaret: aBool [
	showCaret := aBool

]

{ #category : #access }
Paragraph >> string [
	^ text string
]

{ #category : #access }
Paragraph >> text [
	^ text
]

{ #category : #access }
Paragraph >> textOwner: ignored [  "See TextOnCurve"
]

{ #category : #access }
Paragraph >> textStyle [
	^ textStyle
]

{ #category : #access }
Paragraph >> textStyle: aTextStyle [ 
	"Set the style by which the receiver should display its text."
	textStyle := aTextStyle
]

{ #category : #private }
Paragraph >> textStyle: ts lines: l text: t [
	"Private -- just a service for deepCopy"
	textStyle := ts.
	lines := l.
	text := t.
]

{ #category : #access }
Paragraph >> theme [
	^ Smalltalk ui theme
]

{ #category : #access }
Paragraph >> wantsColumnBreaks [

	^wantsColumnBreaks
]

{ #category : #access }
Paragraph >> wantsColumnBreaks: aBoolean [

	wantsColumnBreaks := aBoolean
]
