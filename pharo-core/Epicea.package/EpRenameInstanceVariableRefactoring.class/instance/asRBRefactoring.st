converting
asRBRefactoring
	^ RBRenameInstanceVariableRefactoring 
		rename: oldName 
		to: newName 
		in: affectedClassName