set up
tearDown
	super tearDown.
	RBRefactoringManager instance release.
	RBRefactoringManager classVarNamed: #Instance put: manager.