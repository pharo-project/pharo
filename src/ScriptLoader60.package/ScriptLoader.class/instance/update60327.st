pharo - updates
update60327
	"self new update60327"
	self withUpdateLog: '19467 Metalinks: support accessing variables for arguments
	https://pharo.fogbugz.com/f/cases/19467

19430 Promote DelaySpinScheduler from experimental status (part 1)
	https://pharo.fogbugz.com/f/cases/19430'.
	self loadTogether: self script60327 merge: false.
	
Delay delaySchedulerClass: DelaySpinScheduler.
.
	self flushCaches.
