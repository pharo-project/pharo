"
I add multi-producer/single-consumer semaphore based syncronization to the basic scheduler.
"
Class {
	#name : #DelaySemaphoreScheduler,
	#superclass : #DelayBasicScheduler,
	#instVars : [
		'readyToSchedule',
		'readyToUnschedule'
	],
	#category : #'Kernel-Delays'
}

{ #category : #initialization }
DelaySemaphoreScheduler >> initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap [
	super initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap.
	readyToSchedule := Semaphore new signal.
	readyToUnschedule := Semaphore new signal.

]

{ #category : #initialization }
DelaySemaphoreScheduler >> schedule: aDelay [
	"This is the front-half of scheduling a delay. For back-half see #timingPrioritySchedule:"

	"Only one signal is sent when delay expires. Check if that is already spoken for"
	aDelay beingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	
	readyToSchedule wait.  "Starts signalled. Resignalled from back-end #scheduleAtTimingPriority"
	delayToStart := aDelay.
	timingSemaphore signal. "Transfer execution to back-end #scheduleAtTimingPriority"

]

{ #category : #initialization }
DelaySemaphoreScheduler >> scheduleAtTimingPriority [
	"Private! Invoke only from the timing-priority process.
	 This is the back-half of scheduling a delay. For front-half see #schedule:"

	readyToSchedule signal. "But this timingPriority method completes first"
	super 	scheduleAtTimingPriority.

]

{ #category : #initialization }
DelaySemaphoreScheduler >> unschedule: aDelay [
	readyToUnschedule  wait.  "Starts signalled. Resignalled from back-end #unscheduleAtTimingPriority"
	delayToStop := aDelay.
	timingSemaphore signal. "Transfer execution to back-end #unscheduleAtTimingPriority"
]

{ #category : #initialization }
DelaySemaphoreScheduler >> unscheduleAtTimingPriority [
	"Private! Invoke only from the timing-priority process.
	 This is the back-half of scheduling a delay. For front-half see #schedule:"

	readyToUnschedule signal. "But this timingPriority method completes first"
	super 	unscheduleAtTimingPriority.

]
