tests
testContextWithClosureContextWithOutPointerTesting
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := self class blockClosureContextWithOutPointerTesting.
	self assert: (context1 tempNamed: 'string') = 'test'. 
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	self assert: (materializedContext1 tempNamed: 'string') = 'test'. 
	context1 assertWellMaterializedInto: materializedContext1 in: self.
	
