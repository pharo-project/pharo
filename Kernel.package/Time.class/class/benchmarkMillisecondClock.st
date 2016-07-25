benchmarks
benchmarkMillisecondClock		"Time benchmarkMillisecondClock"
	"Benchmark the time spent in a call to Time>>millisecondClockValue.
	On the VM level this tests the efficiency of calls to ioMSecs()."
	"PII/400 Windows 98: 0.725 microseconds per call"
	| temp1 temp2 temp3 delayTime nLoops time |
	delayTime := 5000. "Time to run benchmark is approx. 2*delayTime"

	"Don't run the benchmark if we have an active delay since
	we will measure the additional penalty in the primitive dispatch
	mechanism (see #benchmarkPrimitiveResponseDelay)."
	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	"Flush the cache for this benchmark so we will have
	a clear cache hit for each send to #millisecondClockValue below"
	Object flushCache.
	temp1 := 0.
	temp2 := self. "e.g., temp1 == Time"
	temp3 := self millisecondClockValue + delayTime.

	"Now check how often we can run the following loop in the given time"
	[temp2 millisecondClockValue < temp3]
		whileTrue:[temp1 := temp1 + 1].

	nLoops := temp1. "Remember the loops we have run during delayTime"

	"Setup the second loop"
	temp1 := 0.
	temp3 := nLoops.

	"Now measure how much time we spend without sending #millisecondClockValue"
	time := Time millisecondClockValue.
	[temp1 < temp3]
		whileTrue:[temp1 := temp1 + 1].
	time := Time millisecondsSince: time.

	"And compute the number of microseconds spent per call to #millisecondClockValue"
	^((delayTime - time * 1000.0 / nLoops) truncateTo: 0.001) printString,
		' microseconds per call to Time>>millisecondClockValue'