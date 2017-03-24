pharo - updates
update60451
	"self new update60451"
	self withUpdateLog: '19863  RBMoveMethodToClassRefactoring reset method protocol
	https://pharo.fogbugz.com/f/cases/19863'.
	self loadTogether: self script60451 merge: false.
	self flushCaches.
