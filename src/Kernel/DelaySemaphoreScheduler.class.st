"
I add multi-producer/single-consumer semaphore based syncronization to the basic scheduler.
"
Class {
	#name : #DelaySemaphoreScheduler,
	#superclass : #DelayBasicScheduler,
	#instVars : [
		'scheduledDelayIsNil',
		'finishedDelayIsNil'
	],
	#category : #'Kernel-Delays'
}

{ #category : #initialization }
DelaySemaphoreScheduler >> initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap [
	super initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap.
	scheduledDelayIsNil := Semaphore new signal.
	finishedDelayIsNil := Semaphore new signal.

]

{ #category : #initialization }
DelaySemaphoreScheduler >> schedule: aDelay [
	scheduledDelayIsNil wait.  
	super schedule: aDelay
]

{ #category : #initialization }
DelaySemaphoreScheduler >> timingPriorityHandleEvent [
	|nowTick|
	"Debug lines to help review. Could be removed after a settling in period"
	
	"When two debuggers appear, step through this higher priority first (or <Proceed>)"
	debug ifTrue: [ self halt ].		
	
	nowTick := ticker nowTick.

	"Handle api-user #schedule: request"
	scheduledDelay ifNotNil: [
		self timingPrioritySchedule: scheduledDelay.
		scheduledDelay := nil.
		scheduledDelayIsNil signal ].

	"Handle api-user #unschedule: request"
	finishedDelay ifNotNil: [
		self timingPriorityUnschedule: finishedDelay.
		finishedDelay := nil.
		finishedDelayIsNil ].

	"Signal any expired delays"
	[ 	activeDelay notNil and: [ nowTick >= activeDelay resumptionTick ]] whileTrue: [
			activeDelay timingPrioritySignalExpired.
			activeDelay := suspendedDelays removeFirstOrNil ].

]

{ #category : #initialization }
DelaySemaphoreScheduler >> unschedule: aDelay [
	finishedDelayIsNil wait.  "signalled from #timingPriorityHandleEvent"
	super unschedule: aDelay
	

]
