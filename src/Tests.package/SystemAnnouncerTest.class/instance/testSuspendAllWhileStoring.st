tests
testSuspendAllWhileStoring
	| value stored |
	
	value := 42.	
	
	stored := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [ SystemAnnouncer uniqueInstance announce: ClassAdded ].
		
	self assert: stored size equals: 1.
	self assert: (stored at:1) equals: ClassAdded. 