event handling
mouseMove: evt
   self paragraph move: evt for: model controller: self editor.
	evt redButtonPressed
		ifFalse: [ ^ self ].
	evt shiftPressed
		ifTrue: [ self modifySelection: (self paragraph characterBlockAtPoint: evt cursorPoint) ]
		ifFalse: [ mouseDownPoint
				ifNotNil: [ self markBlock: (self paragraph characterBlockAtPoint: mouseDownPoint) pointBlock: (self paragraph characterBlockAtPoint: evt cursorPoint) ]
				ifNil: [ self markBlock: (self paragraph characterBlockAtPoint: evt cursorPoint) ] ].
	self editor storeSelectionInText.
