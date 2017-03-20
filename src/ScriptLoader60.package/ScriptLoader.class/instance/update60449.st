pharo - updates
update60449
	"self new update60449"
	self withUpdateLog: '19857 add option #argsAsArray
	https://pharo.fogbugz.com/f/cases/19857

19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809'.
	self loadTogether: self script60449 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
