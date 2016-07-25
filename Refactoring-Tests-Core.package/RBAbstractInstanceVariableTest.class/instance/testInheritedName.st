failure tests
testInheritedName
	self shouldFail: (RBAbstractInstanceVariableRefactoring 
			variable: 'name'
			class: RBBasicLintRuleTest)