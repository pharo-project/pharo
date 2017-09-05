tests
testBasicCheck

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self deprecatedMethodName).

	self assert: (testMethod externalProperties anySatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])