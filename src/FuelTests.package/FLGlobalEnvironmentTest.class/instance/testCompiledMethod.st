tests
testCompiledMethod
	"Any compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."
	
	| classA classB |
	classA := self newClass
		duringTestCompileSilently: 'm  ^ 42';
		yourself.
	classB := self newClass
		duringTestCompileSilently: 'm    ^    42   ';
		yourself.
	self assert: (classA >> #m) bytecodesHash = (classB >> #m) bytecodesHash.
	self deny: (classA >> #m) == (classB >> #m).

	materializationEnvironment 
		at: classA name
		put: classB.

	self assert: (self resultOfSerializeAndMaterialize: classA >> #m) == (classB >> #m)