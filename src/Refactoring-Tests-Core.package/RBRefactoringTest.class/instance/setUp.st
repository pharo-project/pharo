running
setUp
	super setUp.
	manager := RBRefactoringManager classVarNamed: #Instance.
	RBRefactoringManager classVarNamed: #Instance put: nil.
	model := RBClassModelFactory rbNamespace new