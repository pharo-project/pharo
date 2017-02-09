pharo - updates
update60383
	"self new update60383"
	self withUpdateLog: '19677 Cannot parallel trace through same method
	https://pharo.fogbugz.com/f/cases/19677'.
	self loadTogether: self script60383 merge: false.
	self flushCaches.
