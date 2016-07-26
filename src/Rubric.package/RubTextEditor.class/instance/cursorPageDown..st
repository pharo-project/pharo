nonediting/nontyping keys
cursorPageDown: aKeyboardEvent 

	self closeTypeIn.
	self 
		moveCursor: [:position |
			self
				sameColumn: position
				newLine: [:lineNo | lineNo + self pageHeight]
				forward: true]
		forward: true
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true