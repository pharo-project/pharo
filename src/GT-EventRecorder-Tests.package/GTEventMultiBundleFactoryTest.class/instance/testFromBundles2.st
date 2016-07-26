tests
testFromBundles2
	"#fromBundles: can also receive GTMultiBundle object. It should be consolidate too."
	multiBundles := factory fromBundles: self allBundles.
	multiBundles := factory fromBundles: (self allBundles copy asOrderedCollection
		addAll: multiBundles;
		yourself) asArray.
	self assert: multiBundles size equals: 2.
	self detectEvents.
	self assert: multiBundle1 url equals: bundle1 url.
	self deny: multiBundle1 bundles isEmpty.
	self assert: multiBundle2 url equals: bundle4 url.
	self deny: multiBundle2 bundles isEmpty.
	self assert: multiBundle1 bundles size equals: 6.
	self assert: multiBundle2 bundles size equals: 2.
	self assert: multiBundle1 bundles first equals: bundle1.
	self assert: multiBundle1 bundles second equals: bundle2.
	self assert: multiBundle1 bundles third equals: bundle3.
	self assert: multiBundle1 bundles fourth equals: bundle1.
	self assert: multiBundle1 bundles fifth equals: bundle2.
	self assert: multiBundle1 bundles sixth equals: bundle3.
	self assert: multiBundle2 bundles first equals: bundle4.
	self assert: multiBundle2 bundles second equals: bundle4.
