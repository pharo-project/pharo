tests
testContext
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
		
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.