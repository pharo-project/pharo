typing support
handleKeystrokeAction: aBlock
	(aBlock value)
			ifTrue: [self doneTyping.
			self storeSelectionInParagraph.
			^self].
	self openTypeIn.
	self hasSelection 
		ifTrue: [ "save highlighted characters"
			self editingState previousInterval: self selectionInterval selection: self selection].
	self editingState unselect.
	self storeSelectionInParagraph