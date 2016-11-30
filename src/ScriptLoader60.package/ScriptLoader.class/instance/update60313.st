pharo - updates
update60313
	"self new update60313"
	self withUpdateLog: '19406 integrate UFFI v0.25
	https://pharo.fogbugz.com/f/cases/19406

19403 Find and replace dialog first field is unreadable in dark theme
	https://pharo.fogbugz.com/f/cases/19403

17771 Empty stderr/stdout files should be deleted on image shutdown
	https://pharo.fogbugz.com/f/cases/17771'.
	self loadTogether: self script60313 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.25'.self flushCaches.
