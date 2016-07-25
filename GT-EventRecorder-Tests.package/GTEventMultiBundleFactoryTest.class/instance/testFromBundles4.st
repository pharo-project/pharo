tests
testFromBundles4
	"#fromBundles: can also receive GTMultiBundle object. It should be consolidate too."
	multiBundles := factory fromBundles: self allBundles.
	multiBundles := factory fromBundles: multiBundles.
	self assert: multiBundles size equals: 2.
	self detectEvents.
	self assert: multiBundle1 url equals: bundle1 url.
	self deny: multiBundle1 bundles isEmpty.
	self assert: multiBundle2 url equals: bundle4 url.
	self deny: multiBundle2 bundles isEmpty.
	self assert: multiBundle1 bundles size equals: 3.
	self assert: multiBundle2 bundles size equals: 1.
	self assert: multiBundle1 bundles first equals: bundle1.
	self assert: multiBundle1 bundles second equals: bundle2.
	self assert: multiBundle1 bundles third equals: bundle3.
	self assert: multiBundle2 bundles first equals: bundle4.
