Class {
	#name : #DelayIdleTicker,
	#superclass : #DelayMicrosecondTicker,
	#instVars : [
		'timingSemaphore',
		'idle',
		'lastActiveDelay'
	],
	#classVars : [
		'TempDebugIdle'
	],
	#category : #'Kernel-Delays'
}

{ #category : #'as yet unclassified' }
DelayIdleTicker class >> debug: aBoolean [
	"To be removed after code review"
	TempDebugIdle := aBoolean = true
]

{ #category : #'as yet unclassified' }
DelayIdleTicker class >> installExperiment [
	"To be removed after code review"
	ProcessorScheduler startUp.
	Delay delayScheduler: (DelayBasicScheduler onTicker: self new)
]

{ #category : #'as yet unclassified' }
DelayIdleTicker class >> uninstall [
	"To be removed after code review"
	Delay delaySchedulerClass: DelaySemaphoreScheduler.

]

{ #category : #'api-system' }
DelayIdleTicker >> idleAction [
	idle := true.
	timingSemaphore signal.
]

{ #category : #'api-system' }
DelayIdleTicker >> initialize [ 
	"To be removed after code review"
	super initialize.
	TempDebugIdle := false.
]

{ #category : #'api-system' }
DelayIdleTicker >> tempMethod_DebugIdle [
	"To be removed after code review"
	^TempDebugIdle 
]

{ #category : #'api-system' }
DelayIdleTicker >> waitForUserSignalled:  aSemaphore orExpired: activeDelay [
	"Sleep until the active delay is due, or timingSemaphore signalled by delay-scheduler user-api.
	 While waiting, if system becomes idle relinquish the CPU."
	|nextTick debugBeforeWait|
	
	"DelayIdleTicker installExperiment."
	"DelayIdleTicker uninstall."
	"(1 to: 10) collect: [:i | [ (Delay forMilliseconds: 5) wait ] timeToRun asMilliSeconds]."
	
	timingSemaphore := aSemaphore. "needed by #idleAction"

	"debug formatting only"
	(activeDelay = lastActiveDelay) ifFalse: [ Transcript cr ].
	lastActiveDelay := activeDelay.

	nextTick := 	activeDelay 
		ifNil: [ TempDebugIdle ifTrue: [Transcript crShow: '!'].  
			self nowTick + (1"sec" * 1000"msecs" * 1000"usecs") ] 
		ifNotNil: [ TempDebugIdle ifTrue: [Transcript crShow: '+'].	
				activeDelay resumptionTick ].
	TempDebugIdle ifTrue: [Transcript show: 'nt', nextTick printString, 'wait(', (nextTick - self nowTick) printString, ')'].
			
	"Configure wake-up from VM"		
	timingSemaphore initSignals.
	self primSignal: timingSemaphore atUTCMicroseconds: nextTick.

	"Right now system is running this method at highestPriority, so obviously system is not idle this moment"
	idle := false. "may be updated by #idleAction"
		
	debugBeforeWait := self nowTick.
	"WARNING! Stepping <Over> this line may lock the Image. Use <Proceed>."
	timingSemaphore wait. "signals may come from VM, delay-scheduler user-api or #idleAction."	
	TempDebugIdle ifTrue: [Transcript show: ' cpu(', (self nowTick - debugBeforeWait) printString, ')'].

	"If timingSemapore was signalled by #idleAction, relinquish CPU for remaining wait time."
	idle 
		ifTrue: [ |idleTime debugBeforeRelinquish debugActualTimeRelinquished| 
			TempDebugIdle ifTrue: [Transcript show: ' idle-relinquish=>'].
			idleTime := nextTick - self nowTick.
			(idleTime > 0) ifTrue: [ 
				debugBeforeRelinquish := self nowTick. "just for debugging"
				self relinquishProcessorForMicroseconds: idleTime. "also returns on native events"
				debugActualTimeRelinquished := self nowTick - debugBeforeRelinquish.
				TempDebugIdle ifTrue: [Transcript show: idleTime printString, '-', debugActualTimeRelinquished printString, '=', (idleTime - debugActualTimeRelinquished) printString].
				]
			]
]
