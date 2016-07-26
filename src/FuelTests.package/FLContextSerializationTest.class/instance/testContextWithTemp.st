tests
testContextWithTemp
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := (Context newForMethod: FLPair >> #methodWithTemp).
	context1 
		initializeWith: nil 
		stackPtr: 1 
		method: FLPair >> #methodWithTemp 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
	context1 tempNamed: 'string' put: 'capo'.
	
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	self assert: (materializedContext1 tempNamed: 'string') = 'capo'.
	context1 assertWellMaterializedInto: materializedContext1 in: self.