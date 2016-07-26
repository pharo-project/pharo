class initialization
initialize
	"ProcessorScheduler initialize."
	     
	SystemRockBottomPriority := 10.
	SystemBackgroundPriority := 20.
	UserBackgroundPriority := 30.
	UserSchedulingPriority := 40.
	UserInterruptPriority := 50.
	LowIOPriority := 60.
	HighIOPriority := 70.
	TimingPriority := 80.

	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 30.