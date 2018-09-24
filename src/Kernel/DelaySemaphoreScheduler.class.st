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
	"This is the front-half of scheduling a delay. For back-half see #timingPrioritySchedule:"

	"Debug lines to help review. Could be removed after a settling in period"
	debug ifTrue: [ self halt ].
	
	aDelay isScheduled ifTrue: [^self error: 'This Delay has already been scheduled.'].
	
	scheduledDelayIsNil wait.  "Starts signalled. Resignalled from #timingPriorityHandleEvent"
	scheduledDelay := aDelay.
	timingSemaphore signal. 
	debug ifTrue: [ self halt. "If debugger appeared for timingPriority thread, step through that first" ]

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
		finishedDelayIsNil signal ].

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
