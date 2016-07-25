failure tests
testBadMethod
	self shouldFail: (RBAddMethodRefactoring 
			addMethod: 'asdf ^super ^printString'
			toClass: RBBasicLintRuleTest
			inProtocols: #(#accessing ))