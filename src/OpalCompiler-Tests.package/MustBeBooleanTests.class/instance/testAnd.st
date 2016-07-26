tests
testAnd
	| myBooleanObject |
	
	myBooleanObject := MyBooleanObject new.
	self deny: (myBooleanObject and: [true])