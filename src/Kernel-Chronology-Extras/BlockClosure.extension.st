Extension { #name : #BlockClosure }

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> bench [
	"Answer a BenchmarkResult with the number of times the receiver was executed in 5 seconds"

	"[3.14 printString] bench"

	^ self benchFor: 5 seconds
]

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> benchCompareTo: aBlock [
	"Answer a BenchmarkComparison of the receiver to the argument."

	^ self bench compareTo: aBlock bench
]

{ #category : #'*Kernel-Chronology-Extras' }
BlockClosure >> benchFor: duration [

	"Run me for duration and return a BenchmarkResult"

	"[ 100 factorial ] benchFor: 2 seconds"

	| count run exception started |
	count := 0.
	run := true.
	exception := nil. "Used to catch possible exception in the wakeup thread"
	[ [ duration wait. ]
			on: Exception
			do: [ :e | exception := e ].
		run := false ] forkAt: Processor timingPriority - 1.
	started := Time millisecondClockValue.
	[ run ] whileTrue: [
		self value.
		count := count + 1 ].
	exception ifNotNil: [ exception signal ].
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
