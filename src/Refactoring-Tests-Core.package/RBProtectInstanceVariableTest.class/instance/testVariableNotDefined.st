failure tests
testVariableNotDefined
	| refactoring |
	refactoring := RBProtectInstanceVariableRefactoring 
		variable: 'rewrite'
		class: RBSubclassOfClassToRename.
	self shouldFail: refactoring