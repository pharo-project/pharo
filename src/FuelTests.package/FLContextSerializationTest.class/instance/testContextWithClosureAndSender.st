tests
testContextWithClosureAndSender
	"This test should be improved"
	| context1  materializedContext2  context2 |
	
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
	
	context2 := self class blockClosureContextTestingWithSender: context1.
	materializedContext2 := self resultOfSerializeAndMaterialize: context2.
	context2 assertWellMaterializedInto: materializedContext2 in: self.
	
