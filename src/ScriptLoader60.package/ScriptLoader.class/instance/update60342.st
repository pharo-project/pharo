pharo - updates
update60342
	"self new update60342"
	self withUpdateLog: '19531 Latest GTTools integration broke the bootstrap
	https://pharo.fogbugz.com/f/cases/19531'.
	self loadTogether: self script60342 merge: false.
	self flushCaches.
