operations
redoRefactoring: aRBRefactoring

	| aNautilusUI |
	aNautilusUI := NautilusUI on: Nautilus new.

	(NautilusRefactoring model: aNautilusUI) 
		performRefactoring: aRBRefactoring