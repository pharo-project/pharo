failure tests
testMetaClassFailure
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #Foo
			class: RBLintRuleTest class
			subclasses: (Array 
					with: RBBasicLintRuleTest class
					with: RBCompositeLintRuleTest class))