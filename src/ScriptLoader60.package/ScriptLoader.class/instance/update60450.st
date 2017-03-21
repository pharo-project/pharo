pharo - updates
update60450
	"self new update60450"
	self withUpdateLog: '19859 Fix FileList services (readStream to readOnlyStream)
	https://pharo.fogbugz.com/f/cases/19859'.
	self loadTogether: self script60450 merge: false.
	self flushCaches.
