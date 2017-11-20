"
This concrete subclass uses the microsecond primitive & clock, which are based on a 64-bit LargePositiveInteger that rolls over roughly every 50,000 years.

There are some code duplications with simbling classes for performance reasons.

See parent class comment for more info.
"
Class {
	#name : #DelayMicrosecondScheduler,
	#superclass : #DelayScheduler,
	#category : #Kernel-Processes
}

{ #category : #snapshotting }
DelayMicrosecondScheduler >> adjust: aDelay resumptionTimeOldBase: oldBaseTime newBase: newBaseTime [
	"Used to adjust resumption times after a snapshot."

	aDelay schedulerResumptionTime: newBaseTime + ( aDelay schedulerResumptionTime - oldBaseTime).

	"Important! Must only be called from the single timing priority process, e.g...
		DelayScheduler>>saveResumptionTimes 
		DelayScheduler>>restoreResumptionTimes "
]

{ #category : #'timer process' }
DelayMicrosecondScheduler >> handleTimerEvent: microsecondNowTick [
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| microsecondNextTick |

	"Process any schedule requests"
	scheduledDelay ifNotNil: [
		"Schedule the given delay."
		scheduledDelay scheduler: self resumptionTime: microsecondNowTick + (1000 * scheduledDelay millisecondDelayDuration).
		self scheduleDelay: scheduledDelay.
		scheduledDelay := nil ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil ].

	"Signal any expired delays"
	[ activeDelay notNil and:[ microsecondNowTick >= activeDelay schedulerResumptionTime ]] whileTrue: [
		activeDelay schedulerSignalWaitingProcess.
		activeDelay := suspendedDelays removeFirstOrNil.
		].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	microsecondNextTick := microsecondNowTick + (1"sec" * 1000"msecs" * 1000"usecs").
	activeDelay ifNotNil: [
		microsecondNextTick := microsecondNextTick min: activeDelay schedulerResumptionTime ].
	^microsecondNextTick.

]

{ #category : #printing }
DelayMicrosecondScheduler >> millisecondsUntilResumptionTime: microsecondResumptionTime [
	^ (microsecondResumptionTime - Time microsecondClockValue) // 1000
]

{ #category : #primitives }
DelayMicrosecondScheduler >> primSignal: aSemaphore atUTCMicroseconds: aLargePositiveInteger [
	"Signal the semaphore when the microsecondsecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 242>
	^ self primitiveFailed
]

{ #category : #snapshotting }
DelayMicrosecondScheduler >> restoreResumptionTimes [
	"Private! Restore the resumption times of all scheduled Delays after a snapshot or clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| newBaseTime |
	newBaseTime := Time microsecondClockValue.
	suspendedDelays do: [ :delay | 
		self adjust: delay resumptionTimeOldBase: 0 newBase: newBaseTime ].
	activeDelay == nil ifFalse: [
		self adjust: activeDelay resumptionTimeOldBase: 0 newBase: newBaseTime ].

]

{ #category : #'timer process' }
DelayMicrosecondScheduler >> runTimerEventLoop [
	| microsecondNowTick microsecondNextTick   |
	"Run the timer event loop."
	[ runTimerEventLoop ] whileTrue: [ 
		timingSemaphore wait.		
		microsecondNowTick := Time microsecondClockValue.
		microsecondNextTick := self handleTimerEvent: microsecondNowTick.

		"Since we have processed all outstanding requests, reset the timing semaphore so
		that only new work will wake us up again. Do this RIGHT BEFORE setting the next
		wakeup call from the VM because it is only signaled once so we mustn't miss it."
		timingSemaphore consumeAllSignals.
		self primSignal: timingSemaphore atUTCMicroseconds: microsecondNextTick.
		].

]

{ #category : #snapshotting }
DelayMicrosecondScheduler >> saveResumptionTimes [
	"Private! Record the resumption times of all Delays relative to a base time of zero. This is done prior to snapshotting. This method should be called only while the AccessProtect semaphore is held."

	| oldBaseTime |
	oldBaseTime := Time microsecondClockValue. 
	activeDelay == nil
		ifFalse: [
			self adjust: activeDelay resumptionTimeOldBase: oldBaseTime newBase: 0 ].
		
	suspendedDelays do: [ :delay |
		self adjust: delay resumptionTimeOldBase: oldBaseTime newBase: 0 ].

]

{ #category : #'low priority processes' }
DelayMicrosecondScheduler >> schedule: aDelay [
	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	accessProtect critical: [
		scheduledDelay := aDelay.
		timingSemaphore signal. "#handleTimerEvent: sets scheduledDelay:=nil"
		].
	

]

{ #category : #'timer process' }
DelayMicrosecondScheduler >> scheduleDelay: aDelay [
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
DelayMicrosecondScheduler >> shutDown [
	"Suspend the active delay, if any, before snapshotting. It will be reactived when the snapshot is resumed."
	"Details: This prevents a timer interrupt from waking up the active delay in the midst snapshoting, since the active delay will be restarted when resuming the snapshot and we don't want to process the delay twice."
	accessProtect consumeAllSignals.
	self primSignal: nil atUTCMicroseconds: 0.
	self saveResumptionTimes.
	delaySuspended := true.

]

{ #category : #'timer process' }
DelayMicrosecondScheduler >> startTimerEventLoop [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
	
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

{ #category : #snapshotting }
DelayMicrosecondScheduler >> startUp [
	"Restart active delay, if any, when resuming a snapshot."

	"Compare to false since it can be nil"
	(delaySuspended = false) 
		ifTrue:[ ^self error: 'Trying to activate Delay twice' ].
	delaySuspended := false.
	
	self restoreResumptionTimes.
	accessProtect signal.

]

{ #category : #'timer process' }
DelayMicrosecondScheduler >> stopTimerEventLoop [
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
DelayMicrosecondScheduler >> unscheduleDelay: aDelay [
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
