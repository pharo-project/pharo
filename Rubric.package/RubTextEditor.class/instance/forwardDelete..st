typing/selecting keys
forwardDelete: aKeyboardEvent
	"Delete forward over the next character"

	self closeTypeIn.
	self hasSelection
		ifFalse: [ 
			| idx1 idx2 |
			idx1 := self startIndex min: self stopIndex.
			idx2 := self stopIndex max: self startIndex.
			aKeyboardEvent shiftPressed
				ifTrue: [ idx2 := self nextWord: idx1 ]
				ifFalse: [ idx2 := idx2 + 1 ].
			self selectInvisiblyFrom: idx1 to: idx2 - 1 ].
	self addDeleteSelectionUndoRecord.
	self zapSelectionWith: self nullText.
	^ false