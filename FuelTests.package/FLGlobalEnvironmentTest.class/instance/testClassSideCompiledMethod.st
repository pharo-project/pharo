tests
testClassSideCompiledMethod
	"Any class-side compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."
	
	| classA classB |
	classA := self newClass.
	classA class duringTestCompileSilently: 'm  ^ 42'.
	classB := self newClass.
	classB class duringTestCompileSilently: 'm    ^    42   '.
	self assert: (classA class >> #m) bytecodesHash = (classB class >> #m) bytecodesHash.
	self deny: (classA class >> #m) == (classB class >> #m).

	materializationEnvironment 
		at: classA name
		put: classB.

	self assert: (self resultOfSerializeAndMaterialize: classA class >> #m) == (classB class >> #m)