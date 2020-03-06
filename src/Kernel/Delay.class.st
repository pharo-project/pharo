"
I am the main way that a process may pause for some amount of time.  
The simplest usage is like...
	(Delay forSeconds: 5) wait.   
or...
	5 seconds asDelay wait.

An instance of Delay responds to the message 'wait' by suspending the caller's process for a certain amount of time. The duration of the pause is specified when the Delay is created with the message forMilliseconds: or forSeconds:. A Delay can be used again when the current wait has finished. For example, a clock process might repeatedly wait on a one-second Delay.  Delays work across  clock roll-overs.

The maximum possible delay depends on which delay ticker is used:
  * DelayMillisecondTicker uses a 32-bit value that rolls over about every six days, or SmallInteger maxVal // 2 milliseconds.
  * DelayMicrosecondTicker is 64-bit and rolls over every 50,000 years. 

A delay in progress when an image snapshot is suspended and resumed when the snapshot is re-started. 
i.e. from the image perspective of timing, the image snapshot never happened.

"
Class {
	#name : #Delay,
	#superclass : #Object,
	#instVars : [
		'delaySemaphore',
		'beingWaitedOn',
		'millisecondDelayDuration',
		'ticker',
		'resumptionTick'
	],
	#classVars : [
		'Scheduler'
	],
	#category : #'Kernel-Delays'
}

{ #category : #testing }
Delay class >> anyActive [
	"Return true if there is any delay currently active"
		^self scheduler anyActive

]

{ #category : #settings }
Delay class >> delaySchedulerClass [
	^self scheduler class
]

{ #category : #settings }
Delay class >> delaySchedulerClass: aSchedulerClass [ 
	| newScheduler oldScheduler |
	self delaySchedulerClass = aSchedulerClass ifTrue:[ ^self ].
	newScheduler := aSchedulerClass new.
	(newScheduler respondsTo: #startTimerEventLoop) 
		ifFalse: [ self error: 'New delay scheduler must respond to #startTimerEventLoop' ].
	newScheduler startTimerEventLoop.
	"To avoid lockup if a higher priority process preempts this method to schedule a delay,
	the newScheduler needs to be in place before oldScheduler is stopped"
	oldScheduler := self scheduler.
	self scheduler: newScheduler.
	oldScheduler stopTimerEventLoop.
	self inform: 'Delay scheduler set to ' , aSchedulerClass printString.
	
	
]

{ #category : #'instance creation' }
Delay class >> forDuration: aDuration [
 	"Return a new Delay for the given duration."
 	^ self forMilliseconds: aDuration asMilliSeconds
 
]

{ #category : #'instance creation' }
Delay class >> forMilliseconds: aNumber [
	"Return a new Delay for the given number of milliseconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time.
	
	WARNING: There may be a minimal delay time that depends on the system activity and setting of the idle process. See ProcessorScheduler class >> idleTime "

	^ self new setDelay: aNumber forSemaphore: Semaphore new

]

{ #category : #'instance creation' }
Delay class >> forSeconds: aNumber [
	"Return a new Delay for the given number of Seconds"
	^ self forMilliseconds: aNumber * 1000

]

{ #category : #'initialize-release' }
Delay class >> initialize [
	"Delay initialize"
	self scheduler ifNotNil: [ self scheduler stopTimerEventLoop ].
	self scheduler: DelaySemaphoreScheduler new.
	self scheduler startTimerEventLoop. 
	SessionManager default 
		registerSystemClassNamed: self name 
		atPriority: 20.

]

{ #category : #testing }
Delay class >> nextWakeUpTime [

	^ self scheduler nextWakeUpTime.



]

{ #category : #'timer process' }
Delay class >> restartTimerEventLoop [
	self stopTimerEventLoop.
	self startTimerEventLoop.
]

{ #category : #scheduler }
Delay class >> scheduler [

	^ Scheduler.

]

{ #category : #scheduler }
Delay class >> scheduler: anObject [

	Scheduler := anObject.

]

{ #category : #'timer process' }
Delay class >> schedulingProcess [

	^ self scheduler schedulingProcess.

]

{ #category : #snapshotting }
Delay class >> shutDown [

	self scheduler shutDown.

]

{ #category : #'timer process' }
Delay class >> startTimerEventLoop [

	self scheduler startTimerEventLoop.

]

{ #category : #snapshotting }
Delay class >> startUp [
	"Restart active delay, if any, when resuming a snapshot."

	self scheduler startUp.


]

{ #category : #'timer process' }
Delay class >> stopTimerEventLoop [

	^ self scheduler stopTimerEventLoop.

]

{ #category : #settings }
Delay class >> systemSettingOn: aBuilder [
	<systemsettings>
	
	(aBuilder pickOne: #delaySchedulerClass)
		parent: #pharoSystem ;
		target: self;
		label: 'Delay Scheduler' ;
		domainValues: DelayNullScheduler allSubclasses ;
		default: DelaySemaphoreScheduler;
		description: 'Choose the class to use for Delay scheduling.' , 
			String crlf, 'You can observe which is running from Tools > Process Browser.'
	
]

{ #category : #'instance creation' }
Delay class >> timeoutSemaphore: aSemaphore afterMSecs: anInteger [
	"Create and schedule a Delay to signal the given semaphore when the given number of milliseconds has elapsed. Return the scheduled Delay. The timeout can be cancelled by sending 'unschedule' to this Delay."
	"Details: This mechanism is used to provide a timeout when waiting for an external event, such as arrival of data over a network connection, to signal a semaphore. The timeout ensures that the semaphore will be signalled within a reasonable period of time even if the event fails to occur. Typically, the waiting process cancels the timeout request when awoken, then determines if the awaited event has actually occurred."

	^ (self new setDelay: anInteger forSemaphore: aSemaphore) schedule; yourself

]

{ #category : #public }
Delay >> beingWaitedOn [
	"Answer whether this delay is currently scheduled, e.g., being waited on"
	^beingWaitedOn
]

{ #category : #public }
Delay >> delaySemaphore [

	^ delaySemaphore
]

{ #category : #delaying }
Delay >> isExpired [

	^delaySemaphore isSignaled.

]

{ #category : #public }
Delay >> millisecondDelayDuration [
	^millisecondDelayDuration
]

{ #category : #public }
Delay >> millisecondsToGo [
	^ ticker millisecondsUntilTick: resumptionTick
]

{ #category : #printing }
Delay >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: millisecondDelayDuration;
		nextPutAll: ' msecs'.
	beingWaitedOn
		ifTrue: [ 
			aStream
				nextPutAll: '; ';
				print: self millisecondsToGo;
				nextPutAll: ' msecs remaining' ].
	aStream nextPutAll: ')'.
]

{ #category : #public }
Delay >> resumptionTick [
	"The tick semantics (e.g. millisecond/microsecond) depends on the ticker its derived from."
	
	^resumptionTick 

]

{ #category : #public }
Delay >> resumptionTickAdjustFrom: oldBaseTick to: newBaseTick [
	resumptionTick := resumptionTick - oldBaseTick + newBaseTick.  

]

{ #category : #private }
Delay >> schedule [
	"Schedule this delay."
	
	self class scheduler schedule: self.

]

{ #category : #private }
Delay >> setDelay: milliseconds [
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	millisecondDelayDuration := milliseconds asInteger
]

{ #category : #private }
Delay >> setDelay: milliseconds forSemaphore: aSemaphore [
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	millisecondDelayDuration := milliseconds asInteger.
	millisecondDelayDuration < 0 ifTrue: [self error: 'delay times cannot be negative'].
	delaySemaphore := aSemaphore.
	beingWaitedOn := false.
]

{ #category : #'private - timing priority process' }
Delay >> timingPriorityScheduleTicker: aDelayTicker [
	beingWaitedOn ifTrue: [ ^false ]. "Already scheduled"
	beingWaitedOn := true.
	ticker := aDelayTicker. "To help Delay >> printOn: interpret resumptionTick"
	resumptionTick := ticker tickAfterMilliseconds: millisecondDelayDuration.
	^true
]

{ #category : #'private - timing priority process' }
Delay >> timingPrioritySignalExpired [
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	delaySemaphore signal.

	"Important! Must only be called from the single timing priority process e.g...
		DelayScheduler>>handleEventTimer."
]

{ #category : #'private - timing priority process' }
Delay >> timingPriorityUnschedule [
	""

	beingWaitedOn := false.

	"Important! Must only be called from the single timing priority process e.g...
		DelayScheduler>>handleEventTimer."
]

{ #category : #private }
Delay >> unschedule [
	self class scheduler unschedule: self.

]

{ #category : #delaying }
Delay >> wait [
	"Schedule this Delay, then wait on its semaphore. The current process will be suspended for the amount of time specified when this Delay was created."

	self schedule.
	[delaySemaphore wait] ifCurtailed:[self unschedule].

]
