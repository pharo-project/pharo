pharo - updates
update60505
	"self new update60505"
	self withUpdateLog: '20187 Request use of block in certain methods
	https://pharo.fogbugz.com/f/cases/20187

20188 Request representation of integer literal without float exponent
	https://pharo.fogbugz.com/f/cases/20188

20182 Extra dot in literal array
	https://pharo.fogbugz.com/f/cases/20182

18760 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/18760

20186 Request removal of extra statement separators (dot)
	https://pharo.fogbugz.com/f/cases/20186'.
	self loadTogether: self script60505 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
