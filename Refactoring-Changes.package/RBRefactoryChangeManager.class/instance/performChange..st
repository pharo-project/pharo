public access
performChange: aRefactoringChange
	self ignoreChangesWhile: [ self addUndo: aRefactoringChange execute ]