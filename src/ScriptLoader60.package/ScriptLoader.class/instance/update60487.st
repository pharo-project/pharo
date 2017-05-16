pharo - updates
update60487
	"self new update60487"
	self withUpdateLog: '20056 [Easy to review] Odd formatting in the TravisIntegrationHelp
	https://pharo.fogbugz.com/f/cases/20056

19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809

20060 GLMLoggerTest>>testDefault fails if abstract class is abstract in a hierarchy
	https://pharo.fogbugz.com/f/cases/20060'.
	self loadTogether: self script60487 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
