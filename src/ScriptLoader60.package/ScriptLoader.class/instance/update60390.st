pharo - updates
update60390
	"self new update60390"
	self withUpdateLog: '19698 Scoped button with a more meaningful tooltip
	https://pharo.fogbugz.com/f/cases/19698

18760 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/18760

19183 ZnLogEvent open is broken
	https://pharo.fogbugz.com/f/cases/19183'.
	self loadTogether: self script60390 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
