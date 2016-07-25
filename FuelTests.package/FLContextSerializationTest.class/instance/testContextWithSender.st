tests
testContextWithSender
	"This test should be improved"
	| context1 context2 materializedContext1 |
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
		
	context2 := (Context newForMethod: FLPair >> #method2).
	context2 
		initializeWith: 18 
		stackPtr: 1 
		method: FLPair >> #method2 
		receiver:  (FLPair new right: 5; left: 6) 
		sender: nil.

	context1 privSender:  context2.
		
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.