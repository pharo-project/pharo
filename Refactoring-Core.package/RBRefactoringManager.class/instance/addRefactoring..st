public access
addRefactoring: aRefactoring 
	RBRefactoryChangeManager instance performChange: aRefactoring changes.
	refactorings add: aRefactoring class name