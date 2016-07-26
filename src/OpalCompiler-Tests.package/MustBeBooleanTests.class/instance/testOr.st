tests
testOr
	| myBooleanObject |
	
	myBooleanObject := MyBooleanObject new.
	self assert: (myBooleanObject or: [true])