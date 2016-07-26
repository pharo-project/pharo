tests
testBasicCheck1

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName, '1').
		
	self deny: (OverridesDeprecatedMethodRule new basicCheck: testMethod)