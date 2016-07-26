private-extensions-flcontextserializationtest
testContextWithTemp
	"This test should be improved"

	<flExtensionOf: #FLContextSerializationTest>
	| context1 materializedContext1 |
	context1 := (Smalltalk globals at: #MethodContext)
		newForMethod:
			(Smalltalk globals at: #FLPair) >> #methodWithTemp.
	context1
		initializeWith: nil
		stackPtr: 1
		method:
			(Smalltalk globals at: #FLPair) >> #methodWithTemp
		receiver:
			((Smalltalk globals at: #FLPair) new
				right: 4;
				left: 2)
		sender: nil.
	context1 tempNamed: 'string' put: 'capo'.
	materializedContext1 := self
		resultOfSerializeAndMaterialize: context1.
	self assert: (materializedContext1 tempNamed: 'string') = 'capo'.
	context1 assertWellMaterializedInto: materializedContext1 in: self