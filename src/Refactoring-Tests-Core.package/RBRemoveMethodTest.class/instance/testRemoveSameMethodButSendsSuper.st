failure tests
testRemoveSameMethodButSendsSuper
	self shouldWarn: (RBRemoveMethodRefactoring 
			removeMethods: #(#new )
			from: RBBasicLintRuleTest class)