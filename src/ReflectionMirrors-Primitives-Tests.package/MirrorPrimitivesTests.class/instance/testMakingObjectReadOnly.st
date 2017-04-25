tests-write barrier
testMakingObjectReadOnly
	| point |
	point := 10@20.
	MirrorPrimitives makeObject: point readOnly: true.
	self assert: (MirrorPrimitives isObjectReadOnly: point).
	
	[MirrorPrimitives fieldOf: point at: 1 put: 100.
	self assert: false description: 'should raise modification error'] ifError: [].
	self assert: point equals: 10@20.

	MirrorPrimitives makeObject: point readOnly: false.
	self deny: (MirrorPrimitives isObjectReadOnly: point).
	
	MirrorPrimitives fieldOf: point at: 1 put: 100.	
	self assert: point equals: 100@20.