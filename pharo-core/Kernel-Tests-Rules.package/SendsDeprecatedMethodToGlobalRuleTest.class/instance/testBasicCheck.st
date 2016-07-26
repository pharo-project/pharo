tests
testBasicCheck

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self deprecatedMethodName).

	self assert: (SendsDeprecatedMethodToGlobalRule new basicCheck: testMethod)