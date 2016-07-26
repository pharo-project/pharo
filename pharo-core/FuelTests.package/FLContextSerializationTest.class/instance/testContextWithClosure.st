tests
testContextWithClosure
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := self class blockClosureContextTesting.
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.
	
