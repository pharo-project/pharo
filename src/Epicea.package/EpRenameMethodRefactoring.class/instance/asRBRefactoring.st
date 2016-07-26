converting
asRBRefactoring
	^ RBRenameMethodRefactoring
		renameMethod: oldSelector 
		in: affectedClassName 
		to: newSelector 
		permutation: #()