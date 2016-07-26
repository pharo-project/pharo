drawing
drawOnAthensCanvas: anAthensCanvas
	"Send all visible lines to the displayScanner for display"

	| cursorPosition delimiter oppositePos rect previousOppositeBlock |
	cursorPosition := self paragraph pointIndex.
	cursorPosition ifNil: [ ^ self ].
	cursorPosition < 2
		ifTrue: [ ^ self ].
	(self closingDelimiters includes: (delimiter := self paragraph string at: cursorPosition - 1))
		ifFalse: [ 
			(self openingDelimiters includes: (delimiter := self paragraph string at: cursorPosition - 1))
				ifFalse: [ ^ self ] ].
	oppositePos := self positionOfOppositeDelimiter: delimiter startingAt: cursorPosition in: anAthensCanvas clipRect.
	oppositePos = 0
		ifTrue: [ ^ self ].
	previousOppositeBlock := self characterBlockForIndex: oppositePos.
	rect := previousOppositeBlock.
	rect privateSetCorner: (rect corner min: anAthensCanvas clipRect bottomRight) + (0 @ 1).
	rect privateSetOrigin: rect origin.
	anAthensCanvas setPaint: RubTextSelectionColor oppositeDelimiterSelection backgroundColor.
	anAthensCanvas setShape: rect surface expanded.
	anAthensCanvas draw.
	anAthensCanvas setShape: (rect surface insetBy: 0.5).
	(anAthensCanvas setStrokePaint: Color black)
		width: 1.
	anAthensCanvas draw.
