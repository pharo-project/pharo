"
I am the main way that a process may pause for some amount of time.  The simplest usage is like this:

	(Delay forSeconds: 5) wait.   ""or...""
	5 seconds asDelay wait.

An instance of Delay responds to the message 'wait' by suspending the caller's process for a certain amount of time. The duration of the pause is specified when the Delay is created with the message forMilliseconds: or forSeconds:. A Delay can be used again when the current wait has finished. For example, a clock process might repeatedly wait on a one-second Delay.  Delays work across  clock roll-overs.

The maximum possible delay depends on which DelayScheduler is used:
  * DelayMillisecondScheduler uses a 32-bit value that rolls over about every six days, or SmallInteger maxVal // 2 milliseconds.
  * DelayMicrosecondScheduler is 64-bit and rolls over every 50,000 years. 

A delay in progress when an image snapshot is saved and resumed when the snapshot is re-started. 

"
Class {
	#name : #Delay,
	#superclass : #Object,
	#instVars : [
		'delaySemaphore',
		'beingWaitedOn',
		'millisecondDelayDuration',
		'scheduler',
		'schedulerResumptionTime'
	],
	#classVars : [
		'Scheduler'
	],
	#category : #Kernel-Processes
}

{ #category : #testing }
Delay class >> anyActive [
	"Return true if there is any delay currently active"
		^Scheduler anyActive

]

{ #category : #settings }
Delay class >> delaySchedulerClass [
	^Scheduler class
]

{ #category : #settings }
Delay class >> delaySchedulerClass: aSchedulerClass [ 
	| newScheduler |
	self delaySchedulerClass = aSchedulerClass ifTrue:[ ^self ].
	newScheduler := aSchedulerClass new.
	(newScheduler respondsTo: #startTimerEventLoop) 
		ifFalse: [ self error: 'New delay scheduler must respond to #startTimerEventLoop' ].
	Scheduler stopTimerEventLoop.
	Scheduler := newScheduler.
	Scheduler startTimerEventLoop.
	self inform: 'Delay scheduler set to ' , aSchedulerClass printString.
	
	
]

{ #category : #'instance creation' }
Delay class >> forDuration: aDuration [
 	"Return a new Delay for the given duration."
 	^ self forMilliseconds: aDuration asMilliSeconds
 
]

{ #category : #'instance creation' }
Delay class >> forMilliseconds: aNumber [
	"Return a new Delay for the given number of milliseconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time."

	^ self new setDelay: aNumber forSemaphore: Semaphore new

]

{ #category : #'instance creation' }
Delay class >> forSeconds: aNumber [
	"Return a new Delay for the given number of Seconds"
	^ self forMilliseconds: aNumber * 1000

]

{ #category : #initialize-release }
Delay class >> initialize [
	"Delay initialize"
	Scheduler ifNotNil: [ Scheduler stopTimerEventLoop ].
	Scheduler := DelaySpinScheduler new.
	Scheduler startTimerEventLoop. 
	SessionManager default 
		registerSystemClassNamed: self name 
		atPriority: 20.

]

{ #category : #testing }
Delay class >> nextWakeUpTime [

	^ Scheduler nextWakeUpTime.



]

{ #category : #'timer process' }
Delay class >> restartTimerEventLoop [
	self stopTimerEventLoop.
	self startTimerEventLoop.
]

{ #category : #'timer process' }
Delay class >> schedulingProcess [

	^ Scheduler schedulingProcess.

]

{ #category : #snapshotting }
Delay class >> shutDown [

	Scheduler shutDown.

]

{ #category : #'timer process' }
Delay class >> startTimerEventLoop [

	Scheduler startTimerEventLoop.

]

{ #category : #snapshotting }
Delay class >> startUp [
	"Restart active delay, if any, when resuming a snapshot."

	Scheduler startUp.


]

{ #category : #'timer process' }
Delay class >> stopTimerEventLoop [

	^ Scheduler stopTimerEventLoop.

]

{ #category : #settings }
Delay class >> systemSettingOn: aBuilder [
	<systemsettings>
	
	(aBuilder pickOne: #delaySchedulerClass)
		parent: #pharoSystem ;
		target: self;
		label: 'Delay Scheduler' ;
		domainValues: DelayScheduler allSubclasses ;
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

{ #category : #printing }
Delay >> millisecondsToGo [
	"The milli/micro second semantics of /schedulerResumptionTime/ vary with which 
	scheduler set the value, so ask it to interpret."
	^ scheduler millisecondsUntilResumptionTime: schedulerResumptionTime
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

{ #category : #private }
Delay >> schedule [
	"Schedule this delay."
	
	Scheduler schedule: self.

]

{ #category : #'private - scheduler' }
Delay >> scheduler: aDelayScheduler resumptionTime: milliOrMicroResumptionTime [
	"The milli/micro semantics of schedulerResumptionTime varies depending on which 
	scheduler is running, so we record it such that later printOn: can interpret it
	using #millisecondsToGo"
	scheduler := aDelayScheduler.
	schedulerResumptionTime := milliOrMicroResumptionTime.
]

{ #category : #'private - scheduler' }
Delay >> schedulerBeingWaitedOn [
	"Answer whether this delay is currently scheduled, e.g., being waited on"
	^beingWaitedOn
]

{ #category : #'private - scheduler' }
Delay >> schedulerBeingWaitedOn: aBoolean [
	"Indicate whether this delay is currently scheduled, i.e. being waited on"
	
	beingWaitedOn := aBoolean

	"Important! Must only be called from the single timing priority process, e.g...
		DelayScheduler>>scheduleDelay: 
		DelayScheduler>>unscheduleDelay: "

]

{ #category : #'private - scheduler' }
Delay >> schedulerResumptionTime [
	"Answer the value of the system clock at which the receiver's suspended Process will resume.
	The semantics of whether this is a millisecond or microsecond value depends on which scheduler is in use."
	
	^ schedulerResumptionTime 

]

{ #category : #'private - scheduler' }
Delay >> schedulerResumptionTime: anInteger [
	"Private! Should only be called by a subclass of DelayScheduler.
	Set the value of the system's clock at which the receiver's suspended Process will resumed.  The clock semantics depends on the scheduler used e.g. millisecond/microsecond."
	
	schedulerResumptionTime := anInteger
	

]

{ #category : #'private - scheduler' }
Delay >> schedulerSignalWaitingProcess [
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	delaySemaphore signal.

	"Important! Must only be called from the single timing priority process e.g...
		DelayScheduler>>handleEventTimer."
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

{ #category : #private }
Delay >> unschedule [
	Scheduler unschedule: self.

]

{ #category : #delaying }
Delay >> wait [
	"Schedule this Delay, then wait on its semaphore. The current process will be suspended for the amount of time specified when this Delay was created."

	self schedule.
	[delaySemaphore wait] ifCurtailed:[self unschedule].

]
