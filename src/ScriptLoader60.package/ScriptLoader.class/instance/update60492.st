pharo - updates
update60492
	"self new update60492"
	self withUpdateLog: '20065 EpMonitor makes strong subscriptions to the SystemAnnouncer
	https://pharo.fogbugz.com/f/cases/20065'.
	self loadTogether: self script60492 merge: false.
	self flushCaches.
