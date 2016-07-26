*Epicea-Refactorings
asEpiceaEvent
	^ EpRenameInstanceVariableRefactoring 
		rename: variableName
		to: newName
		in: class name