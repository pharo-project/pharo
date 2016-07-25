*Epicea-Refactorings
asEpiceaEvent
	^ EpRenameMethodRefactoring 
		renameMethod: oldSelector 
		in: class name
		to: newSelector