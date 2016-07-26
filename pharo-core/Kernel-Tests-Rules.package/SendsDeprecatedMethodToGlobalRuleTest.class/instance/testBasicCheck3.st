tests
testBasicCheck3

	| testMethod |
	
	testClass addClassVarNamed: #TestVar.
	
	testMethod := testClass >> (
		testClass compile: 'testMethod TestVar yourself').

	self deny: (SendsDeprecatedMethodToGlobalRule new basicCheck: testMethod)