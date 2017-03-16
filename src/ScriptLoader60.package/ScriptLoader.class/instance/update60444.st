pharo - updates
update60444
	"self new update60444"
	self withUpdateLog: '19839 add support for signedByteAt:signedByteAt:put:
	https://pharo.fogbugz.com/f/cases/19839'.
	self loadTogether: self script60444 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.6'.self flushCaches.
