accessing
content: anObject 
	content := anObject trimBoth.
	self addToList: content.
	self listSelectionIndex: (self list indexOf: content).
	self updateList