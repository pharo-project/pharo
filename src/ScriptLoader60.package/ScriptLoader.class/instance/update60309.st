pharo - updates
update60309
	"self new update60309"
	self withUpdateLog: '19406 integrate UFFI v0.25
	https://pharo.fogbugz.com/f/cases/19406

19403 Find and replace dialog first field is unreadable in dark theme
	https://pharo.fogbugz.com/f/cases/19403'.
	self loadTogether: self script60309 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.25'.self flushCaches.
