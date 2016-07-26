failure tests
testBadName
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #'Obje ct'
			class: RBLintRuleTest
			subclasses: (Array 
					with: RBBasicLintRuleTest
					with: RBCompositeLintRuleTest))