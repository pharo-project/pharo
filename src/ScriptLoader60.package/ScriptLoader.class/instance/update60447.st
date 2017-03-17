pharo - updates
update60447
	"self new update60447"
	self withUpdateLog: '19847 removeLink: needs to invalidate
	https://pharo.fogbugz.com/f/cases/19847'.
	self loadTogether: self script60447 merge: false.
	self flushCaches.
