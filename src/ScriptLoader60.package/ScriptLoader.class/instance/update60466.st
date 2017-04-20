pharo - updates
update60466
	"self new update60466"
	self withUpdateLog: '19935 Versionner tags are unreadable in the dark theme
	https://pharo.fogbugz.com/f/cases/19935

19938 Traits methods flattened
	https://pharo.fogbugz.com/f/cases/19938

19809 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19809

19951 StringMorph imageForm shows a white rectangle in DarkTheme
	https://pharo.fogbugz.com/f/cases/19951

19944 Server deployment is broken due to GlobalIdentifier
	https://pharo.fogbugz.com/f/cases/19944'.
	self loadTogether: self script60466 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
