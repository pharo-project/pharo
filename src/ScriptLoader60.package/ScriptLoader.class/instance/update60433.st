pharo - updates
update60433
	"self new update60433"
	self withUpdateLog: '19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809

19799 Nautilus rebuilds the package view twice when adding a new protocol
	https://pharo.fogbugz.com/f/cases/19799

19810 Non weak announcements in the SystemAnnouncer
	https://pharo.fogbugz.com/f/cases/19810'.
	self loadTogether: self script60433 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
