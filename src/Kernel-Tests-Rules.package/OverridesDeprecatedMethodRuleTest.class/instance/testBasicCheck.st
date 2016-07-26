tests
testBasicCheck

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName).
		
	self assert: (OverridesDeprecatedMethodRule new basicCheck: testMethod)