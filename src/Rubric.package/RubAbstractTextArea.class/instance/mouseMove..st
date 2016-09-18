event handling
mouseMove: evt
	evt redButtonPressed
		ifFalse: [ ^ self ].
	evt shiftPressed
		ifTrue: [ self modifySelection: (self paragraph characterBlockAtPoint: evt cursorPoint) ]
		ifFalse: [ mouseDownPoint
				ifNotNil: [ self markBlock: (self paragraph characterBlockAtPoint: evt cursorPoint) pointBlock: (self paragraph characterBlockAtPoint: mouseDownPoint) ]
				ifNil: [ self pointBlock: (self paragraph characterBlockAtPoint: evt cursorPoint) ] ].
	self editor storeSelectionInText.
	mouseDownPoint := nil