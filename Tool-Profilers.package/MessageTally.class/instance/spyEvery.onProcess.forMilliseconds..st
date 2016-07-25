initialize-release
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess := aProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time millisecondClockValue.
	endTime := time0 + msecDuration.
	sem := Semaphore new.
	gcStats := Smalltalk vm getParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer := [
			[
				| startTime |
				startTime := Time millisecondClockValue.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess == Processor preemptedProcess ifTrue: [ observedProcess ] ifFalse: [nil])
					"tally can be > 1 if ran a long primitive"
					by: (Time millisecondClockValue - startTime) // millisecs.
				startTime < endTime
			] whileTrue.
			sem signal.
		] newProcess.
	Timer priority: Processor timingPriority-1.
		"activate the probe and evaluate the block"
	Timer resume.
	"activate the probe and wait for it to finish"
	sem wait.
	"Collect gc statistics"
	Smalltalk vm getParameters keysAndValuesDo: [ :idx :gcVal | 
		gcVal isNumber ifTrue: [ gcStats at: idx put: (gcVal - (gcStats at: idx)) ] ].
	time := Time millisecondClockValue - time0