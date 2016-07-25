profiling
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticksPerMSec := Smalltalk highResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Smalltalk highResClock - ticksPerMSec) 
			// (Time millisecondClockValue - t0).
	"Try to get 10 samples per msec"
	ticks := ticksPerMSec // 10.
	startTime := Time millisecondClockValue.
	vmStats := Smalltalk vm getParameters.
	profilerProcess := [ self runProfilerProcess ] forkAt: Processor timingPriority - 1.
