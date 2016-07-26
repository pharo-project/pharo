tests
testBasicCheck1

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self nonDeprecatedMethodName).

	self deny: (SendsDeprecatedMethodToGlobalRule new basicCheck: testMethod)