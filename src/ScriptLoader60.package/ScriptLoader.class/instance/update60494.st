pharo - updates
update60494
	"self new update60494"
	self withUpdateLog: '20064 ClassTest>>#testMethodsReferencingClass uses non-existing BehaviorTests
	https://pharo.fogbugz.com/f/cases/20064'.
	self loadTogether: self script60494 merge: false.
	self flushCaches.
