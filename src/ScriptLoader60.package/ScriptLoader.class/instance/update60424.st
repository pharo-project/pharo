pharo - updates
update60424
	"self new update60424"
	self withUpdateLog: '19787 Athens crashes the VM
	https://pharo.fogbugz.com/f/cases/19787'.
	self loadTogether: self script60424 merge: false.
	self flushCaches.
