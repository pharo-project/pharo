failure tests
testExistingName
	self
		shouldFail: (RBChildrenToSiblingsRefactoring 
				name: #Object
				class: RBLintRuleTest
				subclasses: (Array 
						with: RBBasicLintRuleTest
						with: RBCompositeLintRuleTest));
		shouldFail: (RBChildrenToSiblingsRefactoring 
				name: #Processor
				class: RBLintRuleTest
				subclasses: (Array 
						with: RBBasicLintRuleTest
						with: RBCompositeLintRuleTest))