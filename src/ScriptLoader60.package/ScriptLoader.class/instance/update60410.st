pharo - updates
update60410
	"self new update60410"
	self withUpdateLog: '19749 FFICallbackTests needs to be skipped for now 
	https://pharo.fogbugz.com/f/cases/19749'.
	self loadTogether: self script60410 merge: false.
	self flushCaches.
