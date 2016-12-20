pharo - updates
update60331
	"self new update60331"
	self withUpdateLog: '18735 DNU on suggestions shortcut in GTDebugger
	https://pharo.fogbugz.com/f/cases/18735'.
	self loadTogether: self script60331 merge: false.
	self flushCaches.
