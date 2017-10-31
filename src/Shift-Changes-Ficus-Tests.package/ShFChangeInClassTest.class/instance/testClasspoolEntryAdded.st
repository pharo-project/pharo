tests
testClasspoolEntryAdded
	| old new changes classpoolChange |
	old := self shFTestClassA.
	new := self shFTestClassAWithClassPoolEntry.
	results := old compareTo: new.

	changes := results select: [ :e | ShFClasspoolChange allSubclasses includes: e class ].
	
	self assert: changes size equals:1.

	classpoolChange := changes at:1.

	self assert: classpoolChange class equals: ShFClasspoolAdded.
	self assert: classpoolChange entryName equals: #AClassPoolEntry.
