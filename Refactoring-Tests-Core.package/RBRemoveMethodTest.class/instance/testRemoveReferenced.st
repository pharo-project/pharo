failure tests
testRemoveReferenced
	self shouldFail: (RBRemoveMethodRefactoring 
			removeMethods: #(#checkClass: )
			from: RBBasicLintRuleTest)