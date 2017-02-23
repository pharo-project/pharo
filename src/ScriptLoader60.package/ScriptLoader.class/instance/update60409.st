pharo - updates
update60409
	"self new update60409"
	self withUpdateLog: '19748 new VMs allow enabling of WriteBarrierTests
	https://pharo.fogbugz.com/f/cases/19748'.
	self loadTogether: self script60409 merge: false.
	self flushCaches.
