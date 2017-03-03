pharo - updates
update60422
	"self new update60422"
	self withUpdateLog: '19785 Athens crashes the VM
	https://pharo.fogbugz.com/f/cases/19785'.
	self loadTogether: self script60422 merge: false.
	self flushCaches.
