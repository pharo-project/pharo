pharo - updates
update60322
	"self new update60322"
	self withUpdateLog: '19447 VersionBrowser always shows single method
	https://pharo.fogbugz.com/f/cases/19447'.
	self loadTogether: self script60322 merge: false.
	self flushCaches.
