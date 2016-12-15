pharo - updates
update60326
	"self new update60326"
	self withUpdateLog: '19341 New class and method tags API
	https://pharo.fogbugz.com/f/cases/19341'.
	self loadTogether: self script60326 merge: false.
	self flushCaches.
