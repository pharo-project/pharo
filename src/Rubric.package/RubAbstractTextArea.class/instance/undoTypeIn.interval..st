multi level undo
undoTypeIn: aText interval: anInterval 
	self handleEdit: [self editor undoTypeIn: aText interval: anInterval]