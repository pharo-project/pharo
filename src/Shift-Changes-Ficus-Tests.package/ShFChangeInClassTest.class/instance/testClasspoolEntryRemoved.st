tests
testClasspoolEntryRemoved
	| old new changes classpoolChange |
	old := self shFTestClassAWithClassPoolEntry.
	new := self shFTestClassA.
	results := old compareTo: new.

	changes := results select: [ :e | ShFClasspoolChange allSubclasses includes: e class ].
	
	self assert: changes size equals:1.

	classpoolChange := changes at:1.

	self assert: classpoolChange class equals: ShFClasspoolRemoved.
	self assert: classpoolChange entryName equals: #AClassPoolEntry.
