Extension { #name : #BlockClosure }

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> bench [
	"Return how many times the receiver can get executed in 5 seconds. 
	Answer a string with meaningful description.
	See #benchFor: which returns a BenchmarkResult"
	
	"[3.14 printString] bench"
	
	| benchmarkResult |
	benchmarkResult := self benchFor: 5 seconds.
	^ benchmarkResult shortPrintString 
]

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> benchFor: duration [
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
]

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> timeToRun [
	"Answer the number of milliseconds taken to execute this block."

	^ Duration milliSeconds: (Time millisecondsToRun: self)
]
