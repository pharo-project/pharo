failure tests
testInvalidSubclass
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #Foo
			class: RBRefactoringTest
			subclasses: (Array 
					with: RBBasicLintRuleTest
					with: RBCompositeLintRuleTest))