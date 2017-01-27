pharo - updates
update60359
	"self new update60359"
	self withUpdateLog: '19588 TestCommandLineHandler should not call exitSuccess
	https://pharo.fogbugz.com/f/cases/19588'.
	self loadTogether: self script60359 merge: false.
	self flushCaches.
