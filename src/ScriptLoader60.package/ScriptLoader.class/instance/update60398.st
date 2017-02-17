pharo - updates
update60398
	"self new update60398"
	self withUpdateLog: '15303 rename MethodContextTest to ContextTest
	https://pharo.fogbugz.com/f/cases/15303'.
	self loadTogether: self script60398 merge: false.
	self flushCaches.
