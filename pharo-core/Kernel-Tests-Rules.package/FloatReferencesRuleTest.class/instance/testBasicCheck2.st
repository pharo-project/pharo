tests
testBasicCheck2

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod').  
	
	self deny: (FloatReferencesRule new basicCheck: testMethod ast)
	 