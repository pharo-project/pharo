pharo - updates
update60386
	"self new update60386"
	self withUpdateLog: '18778 FileList "View as" does not work
	https://pharo.fogbugz.com/f/cases/18778'.
	self loadTogether: self script60386 merge: false.
	self flushCaches.
