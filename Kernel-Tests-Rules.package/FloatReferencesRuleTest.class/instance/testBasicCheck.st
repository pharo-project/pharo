tests
testBasicCheck

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod BoxedFloat64').
	
	self assert: (FloatReferencesRule new basicCheck: testMethod ast)
	 