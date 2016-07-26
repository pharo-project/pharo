tests
testBasicCheck2

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ').

	self deny: (SendsDeprecatedMethodToGlobalRule new basicCheck: testMethod)