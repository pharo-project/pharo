pharo - updates
update60439
	"self new update60439"
	self withUpdateLog: '19822 ScriptingExtensions tests must be standalone
	https://pharo.fogbugz.com/f/cases/19822

19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809'.
	self loadTogether: self script60439 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
