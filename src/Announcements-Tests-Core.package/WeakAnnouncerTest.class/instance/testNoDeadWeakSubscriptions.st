tests
testNoDeadWeakSubscriptions

	5 timesRepeat: [ Smalltalk garbageCollect ].
	
	self assert: (WeakAnnouncementSubscription allSubInstances select: [ :sub | sub subscriber isNil ]) size isZero.
	
	self assert: (LegacyWeakSubscription allSubInstances select: [ :sub | sub subscriber isNil ]) size isZero