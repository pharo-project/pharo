"
DelayExperimentalSemaphoreScheduler addresses a ""Delay scheduling deadlock"" problem of Case 14344 [1] seemingly due to multi-thread suspend/resume operations interacting badly with background morphs, causing the Image to lock up.  See the class comment of DelaySchedulerBackgroundWorkerMorph.

My #schedule: and #unschedule: methods pass Delays from the user priority threads to the high priority #handleTimerEvent thread via  instance variables /scheduledDelay/ and /finishedDelay/.   The original implementation used mutexes to protect these in a shared-memory paradigm.   However it can also be considered a producer-consumer paradigm, so I throw away the mutexes and use semaphores. 

I have been demostrated to work without error,  however but am ""experimental"" due to limited time to test prior to Pharo 4 Release, and also no tests are included.

I can be selected via ""World > System > Settings > Setting > Delay scheduler"".

To compare the different delay schedulers...
   1.   ""World > Tools > Process Browser"", and set to auto-update.
   2.   Transcript open.
   3.   DelayBenchmark runAll.
"
Class {
	#name : #DelayExperimentalSemaphoreScheduler,
	#superclass : #DelayMicrosecondScheduler,
	#instVars : [
		'scheduledDelayIsNil',
		'finishedDelayIsNil'
	],
	#category : #'Morphic-Tests-Processes'
}

{ #category : #'timer process' }
DelayExperimentalSemaphoreScheduler >> handleTimerEvent: microsecondNowTick [
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
		scheduledDelay := nil.
		scheduledDelayIsNil consumeAllSignals. 
		scheduledDelayIsNil signal ].

	"Process any unschedule requests"
	finishedDelay ifNotNil: [
		self unscheduleDelay: finishedDelay.
		finishedDelay := nil.
		finishedDelayIsNil consumeAllSignals.
		finishedDelayIsNil signal ].

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

{ #category : #initialization }
DelayExperimentalSemaphoreScheduler >> initialize [
	super initialize.
	scheduledDelayIsNil := Semaphore new.
	finishedDelayIsNil := Semaphore new.
]

{ #category : #'low priority processes' }
DelayExperimentalSemaphoreScheduler >> schedule: aDelay [

	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].

	scheduledDelay == nil ifFalse: [ scheduledDelayIsNil wait ].  

	"It is assumed that bytecode inlining and the system's co-operative multitasking 
	 prevents interruption before the assignment in the next line."	
		
	scheduledDelay := aDelay. 
	
	"The high priority process this wakes does scheduledDelay:=nil 
	 and signals scheduledDelayNilledSemaphore."
	
	timingSemaphore signal. 
	

]

{ #category : #'low priority processes' }
DelayExperimentalSemaphoreScheduler >> unschedule: aDelay [
	finishedDelay == nil ifFalse: [ finishedDelayIsNil wait ].  

	"It is assumed that bytecode inlining and the system's co-operative multitasking 
	 prevents interruption before the assignment in the next line."	
		
	finishedDelay := aDelay. 
	
	"The high priority process this wakes does scheduledDelay:=nil 
	 and signals scheduledDelayNilledSemaphore."
	
	timingSemaphore signal. 

	

]
