pharo - updates
update60497
	"self new update60497"
	self withUpdateLog: '20084 Highlight Message Send is not enabled by default in bootstrapped/reloaded image
	https://pharo.fogbugz.com/f/cases/20084

20072 Importing Resource Help needs improvement
	https://pharo.fogbugz.com/f/cases/20072

19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809'.
	self loadTogether: self script60497 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
