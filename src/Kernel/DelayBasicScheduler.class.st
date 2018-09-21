"
I schedule delays in a basic way independent of any particular time-base
and without any special multi-threaded synchronisation protection.

I provide the delay scheduling loop running as the system's highest priority process 
waiting for user delay scheduling events or VM events indicating a delay needs to be expired.
I manage suspendedDelays as a Heap sorted by the resumption time of the delay.

My main collaborators are:
Delay - holds the resumptionTick calculated by the ticker
Delay{time-base}Ticker - time-base specific interaction with VM and calculation of resumptionTicks 

USER-API 
#schedule: aDelay
	Forwards delay from user-thread to #timingPrioritySchedule: thread (via signalling #timingPriorityRunEventLoop) 

#unschedule: aDelay
	Forwards delay from user-thread to #timingPriorityUnschedule: thread (via signalling #timingPriorityRunEventLoop) 
	

SYSTEM-API
#startTimerEventLoop{Priority:}
	creates a process runnning the delay scheduling loop, customisable priority to help unit testing

#stopTimeEventLoop
	flags the delay scheduling loop to execute and expire all suspended delays

#shutDown
	suspend the delay scheduling loop to facilite clean snapshot 

#startUp
	unsuspend delay scheduling loop when snapshot resumes


INTERNAL - TIMING PRIORITY FACILITIES
timingPriority is the highest priority process in the system

#timingPriorityRunEventLoop 
	outer loop uses the ticker to wait for VM or user signals
	handles suspend-loop requests from #shutDown 
	invokes the main event handler
	when loop exits, signals all remaining suspendedDelays

#timingPriorityHandleEvent
	the main event handler
	hands delays to the next two methods...

#timingPrioritySchedule: aDelay
   inserts into suspendedDelays and determines activeDelay

#timingPriorityUnSchedule: aDelay
   deletes from suspendedDelays and determines activeDelay


INTERNAL - SYNCHRONISATION
Lacking synchronisation protection I rely solely on the implict semantics of process scheduling.
Specifically, in #schedule:/#unschedule there can be no suspension point between 
the variable assignment and the following #signal which transfers execution to the highest priority process that consumes and clears the variable. In practice this currently works quite well,
but is fragile wrt changing process scheduling semantics.  Subclasses add specific synchronisation
protection.
"
Class {
	#name : #DelayBasicScheduler,
	#superclass : #DelayNullScheduler,
	#instVars : [
		'ticker',
		'runTimerEventLoop',
		'timerEventLoop',
		'suspendedDelays',
		'activeDelay',
		'scheduledDelay',
		'finishedDelay',
		'suspendSemaphore',
		'timingSemaphore',
		'extraSignalCount',
		'loopCount',
		'debug'
	],
	#category : #'Kernel-Delays'
}

{ #category : #'as yet unclassified' }
DelayBasicScheduler class >> defaultSuspendedDelaysHeap [
 ^ Heap 
		sortBlock: [ :delay1 :delay2 | 
			delay1 resumptionTick <= delay2 resumptionTick ].

]

{ #category : #'as yet unclassified' }
DelayBasicScheduler class >> onTicker: aDelayTicker suspendedDelaysHeap: aHeap [
	"Facilitates unit tests examining the otherwise hidden suspendedDelays variable"
	^self basicNew initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap
]

{ #category : #testing }
DelayBasicScheduler >> anyActive [
	"Return true if there is any delay currently active"
	^activeDelay notNil
]

{ #category : #'support - test cases' }
DelayBasicScheduler >> debug: aBoolean [ 
	"Guard against enabling halts in the system's active delay scheduler, which would lock the system"
	(self  schedulingProcess == Delay schedulingProcess) ifTrue: [ ^self ].

	debug := aBoolean
]

{ #category : #initialization }
DelayBasicScheduler >> initialize [
	"Default configuration."

	self initializeTicker: (DelayMicrosecondTicker new) 
		  suspendedDelaysHeap: DelayBasicScheduler defaultSuspendedDelaysHeap.

]

{ #category : #initialization }
DelayBasicScheduler >> initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap [
	ticker := aDelayTicker.
	suspendedDelays := aHeap.
	timingSemaphore := Semaphore new.
	debug := false.

]

{ #category : #deprecated }
DelayBasicScheduler >> millisecondsUntilResumptionTime: microsecondResumptionTime [
	^ (microsecondResumptionTime - Time microsecondClockValue) // 1000
]

{ #category : #testing }
DelayBasicScheduler >> nextWakeUpTime [
	^ activeDelay 
			ifNil: [ 0 ]
			ifNotNil: [ :delay | delay resumptionTick ]
]

{ #category : #printing }
DelayBasicScheduler >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPutAll: '(';
		nextPutAll: self identityHash printString;
		nextPutAll: ') on ' ;
		nextPutAll: ticker className.
		 
]

{ #category : #'system-api' }
DelayBasicScheduler >> restartTimerEventLoop [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	self stopTimerEventLoop.
	self startTimerEventLoop.


]

{ #category : #'user-api' }
DelayBasicScheduler >> schedule: aDelay [
	"This is the front-half of scheduling a delay. For back-half see #timingPrioritySchedule:"

	"Debug lines to help review. Could be removed after a settling in period"
	debug ifTrue: [ self halt ].
	
	aDelay isScheduled ifTrue: [^self error: 'This Delay has already been scheduled.'].
	
	"The basic scheduler is highly dependant on semantics of cooperative multitasking and 
	 bytecode inlining such that interuption *cannot* occur between the following assignment and signal.
	 The #signal immediately transfers execution to timing-priority #runTimerEventLoop
	 such that no other thread can overwrite our value until consumed by timing-priority thread.
	 Subclasses provide addition protections to lessen the importance of these semantics."
	scheduledDelay := aDelay.
	timingSemaphore signal. 
	debug ifTrue: [ self halt. "If debugger appeared for timingPriority thread, step through that first" ]

]

{ #category : #'system-api' }
DelayBasicScheduler >> schedulingProcess [
	^ timerEventLoop 
]

{ #category : #'system-api' }
DelayBasicScheduler >> shutDown [
	"Suspend the timing-priority delay scheduling loop loop before snapshotting
	 to prevent random waking of delayed threads in the midst of snapshoting."
		
	suspendSemaphore := Semaphore new.
	timingSemaphore signal.	
		
	"That #signal transfers execution to timingPriorityRunEventLoop 
	 which then waits for suspendSemaphore to be signalled from #startup"

]

{ #category : #'support - test cases' }
DelayBasicScheduler >> simulate_vmMilliseconds: milliseconds [ 
	"This method is only called from tests which make use of the 'simulation' tickers 
	which implement the next method."
	ticker simulate_vmMilliseconds: milliseconds.

]

{ #category : #'system-api' }
DelayBasicScheduler >> startTimerEventLoop [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	
	self startTimerEventLoopPriority: Processor timingPriority
]

{ #category : #'system-api' }
DelayBasicScheduler >> startTimerEventLoopPriority: processorPriority [
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
	
	runTimerEventLoop := true.
	
	timerEventLoop := [ self timingPriorityRunEventLoop ] newProcess.
	timerEventLoop 
		name: self className , '(' , ticker className, ')';
		priority: processorPriority.
	timerEventLoop resume.

]

{ #category : #'system-api' }
DelayBasicScheduler >> startUp [
	"Unsuspend the timing-priority delay scheduling loop when resuming a snapshot."
	
	suspendSemaphore
		ifNil:[ ^self error: 'Trying to activate Delay twice' ].
	suspendSemaphore signal.
	
	"That #signal immediately transfers execution to timing priority 
	 process #runTimerEventLoop, which does... suspendSemaphore := nil."
]

{ #category : #'system-api' }
DelayBasicScheduler >> stopTimerEventLoop [
	"Stop the timingpriority event loop"	
		
	runTimerEventLoop := false.
	"Transfer execution to #timingPriorityWaitForUserOrDelay:"
	timingSemaphore signal. 
	
	"In normal operation the timingpriority event loop process should already have terminated, 
	but things can get out of order with multi-threaded debuggin. Ensure its terminated." 
	timerEventLoop isTerminated  
		ifFalse: [ 
			debug ifFalse: [ self error: 'Expected timing priority event loop terminated already'].
			timerEventLoop terminate ].
	timerEventLoop := nil.
]

{ #category : #deprecated }
DelayBasicScheduler >> testCaseSupportTimingSemaphore [
	"Tests might alter which sempahore the VM signals, so call this from test tearDown"
	^ timingSemaphore.
]

{ #category : #'system-api' }
DelayBasicScheduler >> ticker [
	^ticker
]

{ #category : #'private - timing priority process' }
DelayBasicScheduler >> timingPriorityHandleEvent [
	|nowTick|
	"Debug lines to help review. Could be removed after a settling in period"
	
	"When two debuggers appear, step through this higher priority first (or <Proceed>)"
	debug ifTrue: [ self halt ].		
	
	nowTick := ticker nowTick.

	"Handle api-user #schedule: request"
	scheduledDelay ifNotNil: [
		self timingPrioritySchedule: scheduledDelay.
		scheduledDelay := nil ].

	"Handle api-user #unschedule: request"
	finishedDelay ifNotNil: [
		self timingPriorityUnschedule: finishedDelay.
		finishedDelay := nil ].

	"Signal any expired delays"
	[ 	activeDelay notNil and: [ nowTick >= activeDelay resumptionTick ]] whileTrue: [
			activeDelay timingPrioritySignalExpired.
			activeDelay := suspendedDelays removeFirstOrNil ].

]

{ #category : #'private - timing priority process' }
DelayBasicScheduler >> timingPriorityRunEventLoop [
	"Private! Call only from the timing-priority process.
	 Outer loop for the timing priority timer event loop.
	 Invoked by api-system #startTimerEventLoop"

	[[runTimerEventLoop] whileTrue: 
		[	"Warning! Stepping <Over> the following line may lock the Image. Use <Into> or <Proceed>."
		 	ticker waitForUserSignalled: timingSemaphore orExpired: activeDelay. 

			"suspendSemaphore set by #shutDown method sets so it can be waited on" 		
			suspendSemaphore ifNotNil: 	
			[	"Warning! Stepping <Over> the following line may lock the Image. Use <Into> or <Proceed>."
				self timingPrioritySuspendEventLoopOn: suspendSemaphore.
				suspendSemaphore := nil
			].
		
			self timingPriorityHandleEvent 
		]
	] ensure: [ "When timer event loop exits, remaining delays are expired."
		debug ifTrue: [ self halt ].
		[activeDelay notNil] whileTrue: [
				activeDelay timingPrioritySignalExpired.
				activeDelay := suspendedDelays removeFirstOrNil ]].
]

{ #category : #'private - timing priority process' }
DelayBasicScheduler >> timingPrioritySchedule: aDelay [
	"Private! Call only from the timing-priority process.
	 This is the back-half of scheduling a delay. For front half see #schedule:"
	
	(aDelay timingPriorityScheduleTicker: ticker) ifFalse:[ ^self ]. "Already scheduled"

	activeDelay 
		ifNil: [ activeDelay := aDelay ]
		ifNotNil: [
			aDelay resumptionTick < activeDelay resumptionTick
				ifTrue: [
					suspendedDelays add: activeDelay.
					activeDelay := aDelay ]
				ifFalse: [ suspendedDelays add: aDelay ]]

]

{ #category : #'private - timing priority process' }
DelayBasicScheduler >> timingPrioritySuspendEventLoopOn: semaphore [
	debug ifTrue: [ self halt ].
	ticker saveResumptionTimes: suspendedDelays asArray, {activeDelay}.
	
	"Warning! Stepping <Over> the following line may lock the Image. Use <Proceed>."
	semaphore wait.
	
	debug ifTrue: [ self halt ].
	ticker restoreResumptionTimes: suspendedDelays asArray, {activeDelay}.  

]

{ #category : #'private - timing priority process' }
DelayBasicScheduler >> timingPriorityUnschedule: aDelay [
	"Private! Call only from the timing-priority process.
	 This is the back-half of unscheduling a delay. For front half see #unschedule:"
	
	aDelay beingWaitedOn ifFalse:[ ^self ].
	
	activeDelay == aDelay 
		ifTrue: [ activeDelay := suspendedDelays removeFirstOrNil ] 
		ifFalse:[ suspendedDelays remove: aDelay ifAbsent: [] ].
	
	aDelay timingPriorityScheduleTicker: nil.
]

{ #category : #'user-api' }
DelayBasicScheduler >> unschedule: aDelay [
	"This is the front-half of unscheduling a delay. For back-half see #timingPriorityUnschedule:"

	"Debug lines to help review. Could be removed after a settling in period"
	debug ifTrue: [ self halt ].

	"The basic scheduler is highly dependant on semantics of cooperative multitasking and 
	 bytecode inlining such that interuption *cannot* occur between the assignment and the signal.
	 The #signal immediately transfers execution to timing-priority #runTimerEventLoop
	 such that no other thread can overwrite our value until consumed by timing-priority thread.
	 Subclasses add protection against this."

	finishedDelay := aDelay.
	
	timingSemaphore signal. "Execution transfers to #timingPriorityWaitForUserOrDelay:"
	debug ifTrue: [ self halt. "If timingPriority thread debugger appears, step through it first" ]


]
