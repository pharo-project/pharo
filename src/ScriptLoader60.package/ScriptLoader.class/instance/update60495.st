pharo - updates
update60495
	"self new update60495"
	self withUpdateLog: '20064 ClassTest>>#testMethodsReferencingClass uses non-existing BehaviorTests
	https://pharo.fogbugz.com/f/cases/20064

20074 Red-pane-of-death when sources file missing
	https://pharo.fogbugz.com/f/cases/20074'.
	self loadTogether: self script60495 merge: false.
	self flushCaches.
