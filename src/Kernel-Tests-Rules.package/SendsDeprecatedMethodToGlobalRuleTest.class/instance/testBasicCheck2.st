tests
testBasicCheck2

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ').

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])