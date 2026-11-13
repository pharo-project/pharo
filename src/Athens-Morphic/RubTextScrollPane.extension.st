Extension { #name : 'RubTextScrollPane' }

{ #category : '*Athens-Morphic' }
RubTextScrollPane >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self textArea readOnly ifTrue: [ ^ self ].
	self drawFocusBorder ifFalse: [ ^ self ].
	self hasKeyboardFocus ifTrue: [ self drawKeyboardFocusOnAthensCanvas: anAthensCanvas ]
]
