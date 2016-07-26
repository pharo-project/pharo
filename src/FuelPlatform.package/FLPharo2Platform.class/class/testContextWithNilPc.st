private-extensions-flcontextserializationtest
testContextWithNilPc
	"This test should be improved"

	<flExtensionOf: #FLContextSerializationTest>
	| context1 materializedContext1 |
	context1 := (Smalltalk globals at: #MethodContext)
		newForMethod:
			(Smalltalk globals at: #FLPair) >> #method1.
	context1
		initializeWith: nil
		stackPtr: 1
		method:
			(Smalltalk globals at: #FLPair) >> #method1
		receiver:
			((Smalltalk globals at: #FLPair) new
				right: 4;
				left: 2)
		sender: nil.
	materializedContext1 := self
		resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self