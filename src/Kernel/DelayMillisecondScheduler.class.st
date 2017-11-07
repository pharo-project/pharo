"
This concrete subclass uses the millisecond primitive & clock, which are based on a 32-bit Integer, rolling over roughly every 6 days.  Special handling for clock rollover is scatter through the implementation.

There are some code duplications with simbling classes for performance reasons.

See parent class comment for more info.
"
Class {
	#name : #DelayMillisecondScheduler,
	#superclass : #DelayScheduler,
	#category : #Kernel-Processes
}

{ #category : #snapshotting }
DelayMillisecondScheduler >> adjust: aDelay resumptionTimeOldBase: oldBaseTime newBase: newBaseTime [
	"Used to adjust resumption times after a snapshot or clock roll-over."

	aDelay schedulerResumptionTime: newBaseTime + ( aDelay schedulerResumptionTime - oldBaseTime).

	"Important! Must only be called from the single timing priority process, e.g...
		DelayScheduler>>saveResumptionTimes 
		DelayScheduler>>restoreResumptionTimes "

	
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> handleTimerEvent: millisecondNowTick [
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| millisecondNextTick |

	"Process any schedule requests"
	scheduledDelay ifNotNil: [
		"Schedule the given delay."
		scheduledDelay scheduler: self resumptionTime: millisecondNowTick + scheduledDelay millisecondDelayDuration.
		self scheduleDelay: scheduledDelay.
		scheduledDelay := nil ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil ].

	"Check for clock wrap-around."
	millisecondNowTick < activeDelayStartTime ifTrue: [
		"clock wrapped"
		self saveResumptionTimes.
		self restoreResumptionTimes ].
	activeDelayStartTime := millisecondNowTick.

	"Signal any expired delays"
	[ activeDelay notNil and:[ millisecondNowTick >= activeDelay schedulerResumptionTime ]] whileTrue: [
		activeDelay schedulerSignalWaitingProcess.
		activeDelay := suspendedDelays removeFirstOrNil.
		].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	millisecondNextTick := millisecondNowTick + 1000"msecs".
	activeDelay ifNotNil: [
		millisecondNextTick := millisecondNextTick min: activeDelay schedulerResumptionTime ].
	millisecondNextTick := millisecondNextTick min: SmallInteger maxVal.
	^millisecondNextTick.
]

{ #category : #printing }
DelayMillisecondScheduler >> millisecondsUntilResumptionTime: millisecondResumptionTime [
	^ millisecondResumptionTime - Time millisecondClockValue
]

{ #category : #primitives }
DelayMillisecondScheduler >> primSignal: aSemaphore atMilliseconds: aSmallInteger [
	"Signal the semaphore when the millisecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 136>
	^self primitiveFailed
]

{ #category : #snapshotting }
DelayMillisecondScheduler >> restoreResumptionTimes [
	"Private! Restore the resumption times of all scheduled Delays after a snapshot or clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| newBaseTime |
	newBaseTime := Time millisecondClockValue.
	suspendedDelays do: [ :delay | 
		self adjust: delay resumptionTimeOldBase: 0 newBase: newBaseTime ].
	activeDelay == nil ifFalse: [
		self adjust: activeDelay resumptionTimeOldBase: 0 newBase: newBaseTime ].
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> runTimerEventLoop [
	| millisecondNowTick millisecondNextTick   |
	"Run the timer event loop."
	[ runTimerEventLoop ] whileTrue: [ 
		timingSemaphore wait.		
		millisecondNowTick := Time millisecondClockValue.
		millisecondNextTick := self handleTimerEvent: millisecondNowTick.

		"Since we have processed all outstanding requests, reset the timing semaphore so
		that only new work will wake us up again. Do this RIGHT BEFORE setting the next
		wakeup call from the VM because it is only signaled once so we mustn't miss it."
		timingSemaphore consumeAllSignals.
		self primSignal: timingSemaphore atMilliseconds: millisecondNextTick.

		"This last test is necessary for the obscure case that the msecs clock rolls over
		after nowTick has been computed (unlikely but not impossible). In this case we'd
		wait for MillisecondClockMask msecs (roughly six days) or until another delay gets
		scheduled (which may not be any time soon). In any case, since handling the
		condition is easy, let's just deal with it"
		Time millisecondClockValue < millisecondNowTick
			ifTrue:[ timingSemaphore signal ]. "retry"
		
		].
]

{ #category : #snapshotting }
DelayMillisecondScheduler >> saveResumptionTimes [
	"Private! Record the resumption times of all Delays relative to a base time of zero. This is done prior to snapshotting or adjusting the resumption times after a clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| oldBaseTime |
	oldBaseTime := Time millisecondClockValue. 
	activeDelay == nil
		ifFalse: [
			oldBaseTime < activeDelayStartTime
				ifTrue: [ oldBaseTime := activeDelayStartTime ].  "clock rolled over"
			self adjust: activeDelay resumptionTimeOldBase: oldBaseTime newBase: 0 ].
		
	suspendedDelays do: [ :delay |
		self adjust: delay resumptionTimeOldBase: oldBaseTime newBase: 0 ].
]

{ #category : #'low priority processes' }
DelayMillisecondScheduler >> schedule: aDelay [
	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	accessProtect critical: [
		scheduledDelay := aDelay.
		timingSemaphore signal. "#handleTimerEvent: sets scheduledDelay:=nil"
		].
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> scheduleDelay: aDelay [
	"Private. Schedule this Delay."
	
	aDelay schedulerBeingWaitedOn: true.
	activeDelay 
		ifNil: [ activeDelay := aDelay ]
		ifNotNil: [
			aDelay schedulerResumptionTime < activeDelay schedulerResumptionTime
				ifTrue: [
					suspendedDelays add: activeDelay.
					activeDelay := aDelay ]
				ifFalse: [ suspendedDelays add: aDelay ]]
]

{ #category : #snapshotting }
DelayMillisecondScheduler >> shutDown [
	"Suspend the active delay, if any, before snapshotting. It will be reactived when the snapshot is resumed."
	"Details: This prevents a timer interrupt from waking up the active delay in the midst snapshoting, since the active delay will be restarted when resuming the snapshot and we don't want to process the delay twice."
	accessProtect consumeAllSignals.
	self primSignal: nil atMilliseconds: 0.
	self saveResumptionTimes.
	delaySuspended := true.
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> startTimerEventLoop [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
	
	activeDelayStartTime := Time millisecondClockValue.
	suspendedDelays := Heap 
			sortBlock: [ :delay1 :delay2 | delay1 schedulerResumptionTime <= delay2 schedulerResumptionTime ].
	timingSemaphore := Semaphore new.
	runTimerEventLoop := true.
	
	timerEventLoop := [ self runTimerEventLoop ] newProcess.
	timerEventLoop 
		name: 'Delay Scheduling Process';
		priority: Processor timingPriority.
	timerEventLoop resume.
	
	timingSemaphore signal. "get going"
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> stopTimerEventLoop [
	"Stop the timer event loop"
	|  suspendedDelayToClear |
	
	runTimerEventLoop := false.
	timingSemaphore signal. 
	
	"The signalled higher priority process should have exited the timer event loop 
	before the next line, but maybe not if that process was suspended, so force termination." 
	timerEventLoop ifNotNil: [ timerEventLoop terminate ]. 
	timerEventLoop := nil.

	"Expire any remaining delays."
	activeDelay ifNotNil: [ activeDelay schedulerSignalWaitingProcess ].
	[ suspendedDelayToClear := suspendedDelays removeFirstOrNil ]
		whileNotNil: [ suspendedDelayToClear schedulerSignalWaitingProcess ].
]

{ #category : #'timer process' }
DelayMillisecondScheduler >> unscheduleDelay: aDelay [
	"Private. Unschedule this Delay."
	aDelay schedulerBeingWaitedOn 
		ifFalse:[ ^self ].
	
	activeDelay == aDelay 
		ifTrue: [
			suspendedDelays isEmpty 
				ifTrue: [ activeDelay := nil ] 
				ifFalse: [ activeDelay := suspendedDelays removeFirst ]] 
		ifFalse:[
			suspendedDelays remove: aDelay ifAbsent: []].
	
	aDelay schedulerBeingWaitedOn: false.
]
