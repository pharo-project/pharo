failure tests
testInheritedName
	self shouldFail: (RBAbstractClassVariableRefactoring 
			variable: #DependentsFields
			class: RBBasicLintRuleTest)