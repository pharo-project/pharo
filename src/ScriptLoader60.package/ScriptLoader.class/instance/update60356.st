pharo - updates
update60356
	"self new update60356"
	self withUpdateLog: '19590 Integrate new Epicea release
	https://pharo.fogbugz.com/f/cases/19590'.
	self loadTogether: self script60356 merge: false.
	self loadConfiguration: 'Epicea' version: '8.0.7'.
EpMonitor current sessionStore resetWithStoreNamed: 'Session'.
	self flushCaches.
