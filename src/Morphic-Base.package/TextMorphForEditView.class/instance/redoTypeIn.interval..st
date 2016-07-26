multi level undo
redoTypeIn: aText interval: anInterval 
	self handleEdit: [self editor redoTypeIn: aText interval: anInterval]