tests
testBasicCheck3

	| testMethod |
	
	testClass addClassVarNamed: #TestVar.
	
	testMethod := testClass >> (
		testClass compile: 'testMethod TestVar yourself').

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])