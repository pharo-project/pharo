"
I am the decorator for highliting opposite character in a text area (like parenthese or square bracket)
"
Class {
	#name : #RubOpeningClosingDelimiterDecorator,
	#superclass : #RubParagraphDecorator,
	#category : 'Rubric-Editing-Core'
}

{ #category : #querying }
RubOpeningClosingDelimiterDecorator class >> key [
	^ #openingClosingDelimitersHighlight
]

{ #category : #drawing }
RubOpeningClosingDelimiterDecorator >> drawOn: aCanvas [
	"Send all visible lines to the displayScanner for display"

	| cursorPosition delimiter oppositePos rect previousOppositeBlock |
	cursorPosition := self paragraph pointIndex ifNil: [ ^ self ].
	cursorPosition < 2 ifTrue: [ ^ self ].
	
	(self closingDelimiters includes: (delimiter := self paragraph string at: cursorPosition - 1)) 	ifFalse: [ 
			(self openingDelimiters includes: (delimiter := self paragraph string at: cursorPosition - 1)) 	ifFalse: [ ^ self ] .
	].

	oppositePos := self positionOfOppositeDelimiter: delimiter startingAt: cursorPosition in: aCanvas clipRect.
	oppositePos = 0 ifTrue: [ ^ self ].
	
	previousOppositeBlock := self characterBlockForIndex: oppositePos.
	rect := previousOppositeBlock.
	rect privateSetCorner: rect corner + (1 @ 1).
	rect privateSetOrigin: rect origin + (-1 @ 0).
	aCanvas fillRectangle: rect color: RubTextSelectionColor oppositeDelimiterSelection backgroundColor.
	aCanvas frameRectangle: rect color: RubTextSelectionColor oppositeDelimiterSelection textColor
]

{ #category : #drawing }
RubOpeningClosingDelimiterDecorator >> drawOnAthensCanvas: anAthensCanvas [
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

]

{ #category : #drawing }
RubOpeningClosingDelimiterDecorator >> positionOfOppositeDelimiter: aDelimiter startingAt: aPosition in: aRectangle [
	| string topLineIndex bottomLineIndex topLineFirst bottomLinelast oppositeDelimiter level here hereChar incr |
	string := self string.
	topLineIndex := self lineIndexForPoint: aRectangle topLeft.
	bottomLineIndex := self lineIndexForPoint: aRectangle bottomRight.
	topLineFirst := (self lines at: topLineIndex) first.
	bottomLinelast := (self lines at: bottomLineIndex) last.
	oppositeDelimiter := self openingDelimiters , self closingDelimiters
		at: (self closingDelimiters , self openingDelimiters indexOf: aDelimiter).
	(self openingDelimiters includes: aDelimiter)
		ifTrue: [ incr := [ :v | v + 1 ] ]
		ifFalse: [ incr := [ :v | v - 1 ] ].
	level := 1.
	here := aPosition - 1.
	[ level > 0 and: [ here > (topLineFirst max: 1) and: [ here <= bottomLinelast ] ] ]
		whileTrue: [ 
			here := incr value: here.
			here > string size
				ifTrue: [ ^ 0 ].
			hereChar := string at: here.
			hereChar = oppositeDelimiter
				ifTrue: [ 
					level := level - 1.
					level = 0
						ifTrue: [ ^ here ] ]
				ifFalse: [ 
					hereChar = aDelimiter
						ifTrue: [ level := level + 1 ] ] ].
	^ 0
]
