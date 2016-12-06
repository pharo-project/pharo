pharo - updates
update60318
	"self new update60318"
	self withUpdateLog: '19425 Stack underflow with meta links on message nodes
	https://pharo.fogbugz.com/f/cases/19425

19284 MetaLink: re-enabling a condition with arguments can not be executed anymore
	https://pharo.fogbugz.com/f/cases/19284

18994 Komitter should stringify the commit message (breaks GitFileTree)
	https://pharo.fogbugz.com/f/cases/18994

19421 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19421

19415 classVariableAdd and classVariableRemove not announce ClassModificationApplied event
	https://pharo.fogbugz.com/f/cases/19415'.
	self loadTogether: self script60318 merge: false.
	
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
