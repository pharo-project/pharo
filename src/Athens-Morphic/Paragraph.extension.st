Extension { #name : 'Paragraph' }

{ #category : '*Athens-Morphic' }
Paragraph >> displayCaretAndSelectionOnAthensCanvas: anAthensCanvas [

	self selectionStart ifNil: [ ^ self ].
	self selectionStart = self selectionStop
		ifTrue: [ self drawCaretOnAthensCanvas: anAthensCanvas ]
		ifFalse: [
				| selection |
				self caretRect: nil.
				selection := ParagraphSelectionBlock first: self selectionStart last: self selectionStop color: self selectionColor.
				self forLinesIn: anAthensCanvas clipRect do: [ :line | selection displayInLine: line onAthensCanvas: anAthensCanvas ] ]
]

{ #category : '*Athens-Morphic' }
Paragraph >> displayExtraSelectionOnAthensCanvas: anAthensCanvas [
	| visibleRectangle |
	visibleRectangle := anAthensCanvas clipRect.
	self refreshExtraSelection = true
		ifTrue: [ self buildSelectionBlocksFrom: visibleRectangle topLeft to: visibleRectangle bottomRight.
			self refreshExtraSelection: false ].

	self extraSelectionBlocks
		ifNotNil: [
			self forLinesIn: visibleRectangle	do: [ :line |
					self extraSelectionBlocks
						do: [ :selblock | self displaySelectionBlock: selblock inLine: line onAthensCanvas: anAthensCanvas ] ] ]
]

{ #category : '*Athens-Morphic' }
Paragraph >> displaySelectionBarOnAthensCanvas: anAthensCanvas [

	| visibleRectangle line |
	self selectionStart ifNil: [ ^ self ].
	self selectionStop ifNil: [ ^ self ].
	visibleRectangle := anAthensCanvas clipRect.
	self selectionStart textLine = self selectionStop textLine ifFalse: [ ^ self ].
	line := self selectionStart textLine.
	anAthensCanvas setPaint: self selectionBarColor.
	anAthensCanvas drawShape: (visibleRectangle left @ line top corner: visibleRectangle right @ line bottom)
]

{ #category : '*Athens-Morphic' }
Paragraph >> displaySelectionBlock: aSelBlock inLine: line onAthensCanvas: anAthensCanvas [

	(self selectionStart isNotNil and: [ self selectionStop isNotNil and: [ self selectionStart ~= self selectionStop ] ]) ifTrue: [
			| startIdx stopIdx selSartIdx selStopIdx selBlockRange selRange |
			startIdx := aSelBlock first stringIndex.
			stopIdx := aSelBlock last stringIndex.
			selSartIdx := self selectionStart stringIndex.
			selStopIdx := self selectionStop stringIndex.
			selBlockRange := startIdx to: stopIdx.
			selRange := selSartIdx to: selStopIdx.
			((selBlockRange rangeIncludes: selSartIdx + 1) or: [
					 (selBlockRange rangeIncludes: selStopIdx - 1) or: [
						 (selRange rangeIncludes: startIdx + 1) or: [ selRange rangeIncludes: stopIdx - 1 ] ] ]) ifTrue: [ ^ self ] ]. "regular selection and this selection block overlap"
	aSelBlock displayInLine: line onAthensCanvas: anAthensCanvas
]

{ #category : '*Athens-Morphic' }
Paragraph >> displayTextOnAthensCanvas: anAthensCanvas in: aRectangle [

	| athensDisplayScanner offset leftInRun |
	anAthensCanvas clipBy: aRectangle during: [
			anAthensCanvas pathTransform restoreAfter: [
					offset := aRectangle topLeft - self positionWhenComposed.
					athensDisplayScanner := AthensDisplayScanner for: self on: anAthensCanvas.
					leftInRun := 0.
					self forLinesIn: anAthensCanvas clipRect do: [ :line |
						line first <= line last ifTrue: [ leftInRun := athensDisplayScanner displayLine: line offset: offset leftInRun: leftInRun ] ] ] ]
]

{ #category : '*Athens-Morphic' }
Paragraph >> drawCaretOnAthensCanvas: anAthensCanvas [

	(self showCaret not or: [ self selectionStart isNil ]) ifTrue: [ ^ self ].
	self caretRect: (self selectionStart topLeft asIntegerPoint extent: 1 @ self selectionStart height).
	anAthensCanvas setPaint: self insertionPointColor.
	anAthensCanvas drawShape: self caretRect
]

{ #category : '*Athens-Morphic' }
Paragraph >> drawOnAthensCanvas: aCanvas bounds: aRectangle [
	"do nothing"
]

{ #category : '*Athens-Morphic' }
Paragraph >> drawOnAthensCanvas: anAthensCanvas bounds: aRectangle color: aColor [
	self displayCaretAndSelectionOnAthensCanvas: anAthensCanvas.
	self displayExtraSelectionOnAthensCanvas: anAthensCanvas.
	anAthensCanvas setPaint: aColor.
	self displayTextOnAthensCanvas: anAthensCanvas in: aRectangle
]

{ #category : '*Athens-Morphic' }
Paragraph >> forLinesIn: aVisibleRect do: aBlock [

	(self lineIndexForPoint: aVisibleRect topLeft) to: (self lineIndexForPoint: aVisibleRect bottomRight) do: [ :i |
	aBlock value: (self basicLines at: i) ]
]
