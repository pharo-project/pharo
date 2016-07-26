failure tests
testExistingSelector
	self shouldFail: (RBAddMethodRefactoring 
			addMethod: 'printString ^super printString'
			toClass: RBBasicLintRuleTest
			inProtocols: #(#accessing ))