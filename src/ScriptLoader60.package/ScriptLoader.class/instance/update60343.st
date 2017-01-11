pharo - updates
update60343
	"self new update60343"
	self withUpdateLog: '19532 Improve TBehaviour >> lookupSelector:
	https://pharo.fogbugz.com/f/cases/19532

19501 GlobalIdentifierTest leaves a file present in $data directory
	https://pharo.fogbugz.com/f/cases/19501'.
	self loadTogether: self script60343 merge: false.
	self flushCaches.
