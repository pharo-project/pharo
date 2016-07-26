testing
testPoolVariableAccessibleInClassUser
	"This test shows that a Pool Variable is accessible from the class that declare the Pool usage: here the superclass"
	"self debug: #testInClassUser"
	PoolDefiner initialize.
	RootClassPoolUser compileAll.
	
	self assert: RootClassPoolUser gloups = 42.
	self assert: RootClassPoolUser author = 'Ducasse'