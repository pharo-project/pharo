tests
testBasicCheck1

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod SmallFloat64').  
	
	self assert: (FloatReferencesRule new basicCheck: testMethod ast)
	 