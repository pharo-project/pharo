tests
testRegistryShouldBeCleaned

	| oldSize |
	"We force some garbage collection to avoid noise in the test"
	3 timesRepeat: [Smalltalk garbageCollect].
	oldSize := File registry size.

	(File named: 'asd.txt') writeStream.
	3 timesRepeat: [Smalltalk garbageCollect].
	
	self assert: oldSize equals: File registry size.