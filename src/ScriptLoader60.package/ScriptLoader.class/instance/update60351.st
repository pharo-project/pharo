pharo - updates
update60351
	"self new update60351"
	self withUpdateLog: '19534 Unify Epicea UIs
	https://pharo.fogbugz.com/f/cases/19534'.
	self loadTogether: self script60351 merge: false.
	self loadConfiguration: 'Epicea' version: '8.0.6'.
EpMonitor current sessionStore subscribeToSnapshotDone.
	self flushCaches.
