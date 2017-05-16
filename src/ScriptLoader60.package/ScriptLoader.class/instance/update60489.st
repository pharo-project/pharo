pharo - updates
update60489
	"self new update60489"
	self withUpdateLog: '20059 Epicea: Integrate release 8.2.3
	https://pharo.fogbugz.com/f/cases/20059'.
	self loadTogether: self script60489 merge: false.
	self loadConfiguration: 'Epicea' version: '8.2.3'.self flushCaches.
