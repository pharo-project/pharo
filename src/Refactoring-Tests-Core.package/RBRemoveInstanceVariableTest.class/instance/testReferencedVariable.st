failure tests
testReferencedVariable
	self shouldFail: (RBRemoveInstanceVariableRefactoring 
			variable: 'name'
			class: RBLintRuleTest)