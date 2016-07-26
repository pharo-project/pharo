evaluating
benchFor: duration
	"Run me for duration and return a BenchmarkResult"
	
	"[ 100 factorial ] benchFor: 2 seconds"
	
	| count run started |
	count := 0.
	run := true.
	[ duration wait. run := false ] forkAt: Processor timingPriority - 1.
	started := Time millisecondClockValue.
	[ run ] whileTrue: [ self value. count := count + 1 ].
	^ BenchmarkResult new 
		iterations: count; 
		elapsedTime: (Time millisecondsSince: started) milliSeconds; 
		yourself