testing
testPoolVariableAccessibleInSubclassOfClassUser
	"This test shows that a Pool Variable is not accessible from a subclass that declare the Pool usage: here SubFlop subclass of Flop and this is a bug. "
	"self debug: #testInSubclassOfClassUser"
	PoolDefiner initialize.
	SubclassPoolUser compileAll.
	
	self assert: SubclassPoolUser gloups = 42.
	self assert: SubclassPoolUser author = 'Ducasse'