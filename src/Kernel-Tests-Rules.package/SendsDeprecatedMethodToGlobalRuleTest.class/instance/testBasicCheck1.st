tests
testBasicCheck1

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self nonDeprecatedMethodName).

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])