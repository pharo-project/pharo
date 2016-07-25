profiling
stopProfiling
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime := Time millisecondClockValue - startTime.
	Smalltalk vm getParameters keysAndValuesDo: [ :idx :value | 
		value isInteger ifTrue: [ vmStats at: idx put: (value - ((vmStats at: idx) ifNil: [ 0 ])) ].
	].
	profilerProcess ifNotNil: [
		profilerProcess suspend.
		profilerProcess := nil.
	].
