tests
testNoDeadSubscriptions

	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	self deny: (
		(SystemAnnouncer uniqueInstance subscriptions instVarNamed: #subscriptions) anySatisfy: [ :sub | 
		sub subscriber isNil ]
	)