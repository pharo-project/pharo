pharo - updates
update60338
	"self new update60338"
	self withUpdateLog: '19514 ConfigurationOfFuel has no latest FuelTests version
	https://pharo.fogbugz.com/f/cases/19514

19421 Failing test: WeakAnnouncerTest>>#testNoDeadWeakSubscriptions
	https://pharo.fogbugz.com/f/cases/19421

19512 References to Tab -> should be TabMorph
	https://pharo.fogbugz.com/f/cases/19512

18729 DNU on showing menu in CriticBrowser (code pane)
	https://pharo.fogbugz.com/f/cases/18729'.
	self loadTogether: self script60338 merge: false.
	self loadConfiguration: 'Fuel' version: '2.1.10'.
(SystemAnnouncer uniqueInstance subscriptions glmSubscriptions select: [:sub | sub subscriber isNil]) do: #finalize. 3 timesRepeat: [Smalltalk garbageCollect ].
	self flushCaches.
