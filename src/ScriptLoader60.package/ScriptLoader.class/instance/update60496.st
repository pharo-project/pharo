pharo - updates
update60496
	"self new update60496"
	self withUpdateLog: '20068 Change sources file name for bootstrapped image
	https://pharo.fogbugz.com/f/cases/20068'.
	self loadTogether: self script60496 merge: false.
	self flushCaches.
