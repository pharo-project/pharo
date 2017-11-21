"
I provide the high priority process that interacts with the VM via /timingSempahore/ to manage scheduling of Delays.  

The original pre-2014 code on the class-side of Delay was refactored to a singleton pattern in DelayScheduler, then further refactored into two subclasses...
 * DelayMillisecondScheduler (pre-2014, 6 day clock rollover)
 * DelayMicrosecondScheduler (new, 50,000 year rollover)
A fair amount of code is duplicated in the subclasses for both performance and helping with the live transition to the new code (some cleanup required after the new code has been in use for a while)

You can switch between different DelayScheduler implementations using Settings Browser > System. 
You can observe which delay scheduler is running using Tools > Process Browser.

Original word of advice:
DelayScheduler is THE highest priority code which is run in Pharo.  In other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system. Don't fix it if it ain't broken! This code isn't supposed to be beautiful, it's supposed to be fast! The reason for duplicating code is to make it fast. The reason for not using ifNil:[]ifNotNil:[] is that the compiler might not inline those. Since the effect of changes are VERY hard to predict it is best to leave things as they are for now unless there is an actual need to change anything.

Revised word of advice:
Now you can swap between different schedulers on the fly, have a go. Create your own subclass of  DelayScheduler and use DelayBenchmark to compare the alternatives.  

Tip: If while experimenting you have trouble with the UI locking (which prevents debugging), in WorldState>>doOneCycleFor: comment out the call to #interCyclePause: .


"
Class {
	#name : #DelayScheduler,
	#superclass : #Object,
	#instVars : [
		'accessProtect',
		'activeDelay',
		'activeDelayStartTime',
		'delaySuspended',
		'finishedDelay',
		'runTimerEventLoop',
		'scheduledDelay',
		'suspendedDelays',
		'timerEventLoop',
		'timingSemaphore'
	],
	#category : #Kernel-Processes
}

{ #category : #snapshotting }
DelayScheduler >> adjust: aDelay resumptionTimeOldBase: oldBaseTime newBase: newBaseTime [
	self subclassResponsibility 
]

{ #category : #testing }
DelayScheduler >> anyActive [
	"Return true if there is any delay currently active"
	^activeDelay notNil
]

{ #category : #'timer process' }
DelayScheduler >> handleTimerEvent: nowTick [
	self subclassResponsibility 
]

{ #category : #'class initialization' }
DelayScheduler >> initialize [
	"Delay initialize"
	
	"Get the current timing semaphore, or install it if there is none"
	timingSemaphore := (Smalltalk specialObjectsArray at: 30) ifNil: [ 
		Smalltalk specialObjectsArray at: 30 put: Semaphore new ].
	
	delaySuspended := true.
	accessProtect := Semaphore forMutualExclusion.

	"Case 14353 - The following is moving here from #startTimerEventLoop to help with tests. 
	Its also left in its original location for now, to be cleaned up later."
	suspendedDelays := Heap 
			sortBlock: [ :delay1 :delay2 | delay1 schedulerResumptionTime <= delay2 schedulerResumptionTime ].

]

{ #category : #printing }
DelayScheduler >> millisecondsUntilResumptionTime: millisecondResumptionTime [
	self subclassResponsibility 
]

{ #category : #testing }
DelayScheduler >> nextWakeUpTime [
	^ accessProtect
		critical: [ activeDelay 
				ifNil: [ 0 ]
				ifNotNil: [ activeDelay schedulerResumptionTime ]]
]

{ #category : #'timer process' }
DelayScheduler >> restartTimerEventLoop [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	self stopTimerEventLoop.
	self startTimerEventLoop.


]

{ #category : #snapshotting }
DelayScheduler >> restoreResumptionTimes [
	self subclassResponsibility 
]

{ #category : #'timer process' }
DelayScheduler >> runTimerEventLoop [
	self subclassResponsibility 
]

{ #category : #snapshotting }
DelayScheduler >> saveResumptionTimes [
	self subclassResponsibility 
]

{ #category : #'low priority processes' }
DelayScheduler >> schedule: aDelay [
	self subclassResponsibility 
]

{ #category : #'timer process' }
DelayScheduler >> scheduleDelay: aDelay [
	self subclassResponsibility 
]

{ #category : #'timer process' }
DelayScheduler >> schedulingProcess [
	^ timerEventLoop 
]

{ #category : #snapshotting }
DelayScheduler >> shutDown [
	self subclassResponsibility 
]

{ #category : #'timer process' }
DelayScheduler >> startTimerEventLoop [
	self subclassResponsibility 
]

{ #category : #snapshotting }
DelayScheduler >> startUp [
	"Restart active delay, if any, when resuming a snapshot."

	"Compare to false since it can be nil"
	(delaySuspended = false) 
		ifTrue:[ ^self error: 'Trying to activate Delay twice' ].
	delaySuspended := false.
	
	activeDelayStartTime := Time millisecondClockValue.
	self restoreResumptionTimes.
	accessProtect signal.

]

{ #category : #'timer process' }
DelayScheduler >> stopTimerEventLoop [
	self subclassResponsibility 
]

{ #category : #'low priority processes' }
DelayScheduler >> unschedule: aDelay [
	accessProtect critical: [
		finishedDelay := aDelay.
		timingSemaphore signal.
		].


]

{ #category : #'timer process' }
DelayScheduler >> unscheduleDelay: aDelay [
	self subclassResponsibility 
]
