private-extensions-flcontextserializationtest
testContextWithSender
	"This test should be improved"

	<flExtensionOf: #FLContextSerializationTest>
	| context1 context2 materializedContext1 |
	context1 := (Smalltalk globals at: #MethodContext)
		newForMethod:
			(Smalltalk globals at: #FLPair) >> #method1.
	context1
		initializeWith: 23
		stackPtr: 1
		method:
			(Smalltalk globals at: #FLPair) >> #method1
		receiver:
			((Smalltalk globals at: #FLPair) new
				right: 4;
				left: 2)
		sender: nil.
	context2 := (Smalltalk globals at: #MethodContext)
		newForMethod:
			(Smalltalk globals at: #FLPair) >> #method2.
	context2
		initializeWith: 18
		stackPtr: 1
		method:
			(Smalltalk globals at: #FLPair) >> #method2
		receiver:
			((Smalltalk globals at: #FLPair) new
				right: 5;
				left: 6)
		sender: nil.
	context1 privSender: context2.
	materializedContext1 := self
		resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self