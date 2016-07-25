private-extensions-flcontextserializationtest
testContextWithClosureAndSender
	"This test should be improved"

	<flExtensionOf: #FLContextSerializationTest>
	| context1 materializedContext2 context2 |
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
	context2 := self class blockClosureContextTestingWithSender: context1.
	materializedContext2 := self
		resultOfSerializeAndMaterialize: context2.
	context2 assertWellMaterializedInto: materializedContext2 in: self