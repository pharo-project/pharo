initialize-release
initialize
	"Delay initialize"
	Scheduler ifNotNil: [ Scheduler stopTimerEventLoop ].
	Scheduler := DelayExperimentalSpinScheduler new.
	Scheduler startTimerEventLoop. 
	SessionManager default 
		registerSystemClassNamed: self name 
		atPriority: 20.
