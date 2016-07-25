nonediting/nontyping keys
cursorPageUp: aKeyboardEvent 

	self closeTypeIn.
	self 
		moveCursor: [:position |
			self
				sameColumn: position
				newLine: [:lineNo | lineNo - self pageHeight]
				forward: false]
		forward: false
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true