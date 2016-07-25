profiling
runProfilerProcess
	"Run the profiler process"
	| process tallyStart tallyTicks tallyStop method leaf |
	tallyRoot := QSystemTally new class: nil method: nil.
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Smalltalk highResClock.
	[ true ] whileTrue: [
		tallyStart := tallyStop.
		tallyStart := Smalltalk highResClock.
		Smalltalk profileStart: ticks. "run for n ticks"
		semaphore wait.
		tallyStop := Smalltalk highResClock.
		tallyTicks := tallyStop - tallyStart.
		totalTicks := totalTicks + tallyTicks.
		process := Smalltalk profileSample.
		method := Smalltalk profilePrimitive.
		totalTally := totalTally + 1.
		process ifNotNil:[
			leaf := tallyRoot tally: (process suspendedContext ifNil: [ thisContext ] ) by: tallyTicks.
			method ifNotNil: [ leaf tallyMethod: method by: tallyTicks ].
		].
	].
