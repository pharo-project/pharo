accessing
content: anObject 
	content := anObject.
	self model perform: addToListSel with: content.
	self listSelectionIndex: (self list indexOf: content).
	self updateList