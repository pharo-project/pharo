tests
testCompiledMethodChanged
	"A compiled methods should be serialized as global by default. On materialization, it must be found in the global environment, and the bytecodes hash must be the same. Else, raise a proper error."
	
	| classA classB |
	classA := self newClass
		duringTestCompileSilently: 'm  ^ 42';
		yourself.
	classB := self newClass
		duringTestCompileSilently: 'm  ^ [ 42 ]';
		yourself.

	materializationEnvironment 
		at: classA name
		put: classB.

	self serialize: classA >> #m.

	self should: [ self materialization ]
		raise: FLMethodChanged 
		description: 'Serialized and materialized methods should have the same bytecodes.'