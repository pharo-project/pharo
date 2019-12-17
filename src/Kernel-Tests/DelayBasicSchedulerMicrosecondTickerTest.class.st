"
I specify the tests for the delay scheduler system. My subclasses inherit 
these tests to run against various combinations of scheduler and ticker.   

The scheduler is specified by #classForScheduler.
The ticker is specified by #classForTicker.

I specifically test...
  scheduler DelayBasicScheduler together with its default 
  ticker DelayMicrosecondTicker.

My #setUp method starts a new scheduler process running at a priority slightly higher than 
the tests, so that it will preempt the tests similar to how the system's current delay scheduler
running at timingPriority will normally preempt all other processes.  This is provided
to tests in the /scheduler/ variable.
"
Class {
	#name : #DelayBasicSchedulerMicrosecondTickerTest,
	#superclass : #TestCase,
	#instVars : [
		'ticker',
		'scheduler',
		'suspendedDelaysHeap'
	],
	#classVars : [
		'DebugMe'
	],
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> classForScheduler [
	^ DelayBasicScheduler
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> classForTicker [
	^ DelayMicrosecondTickerSimulation
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> setDebugOnOff [
	"highlight and evaluate either of the following lines as needed" 
		DebugMe := true.
		DebugMe := false.
]

{ #category : #running }
DelayBasicSchedulerMicrosecondTickerTest >> setUp [
	"Event loop priority takes So it takes priority over following code"
	super setUp.
	ticker := self classForTicker new.
	suspendedDelaysHeap := self classForScheduler defaultSuspendedDelaysHeap.
	scheduler := self classForScheduler onTicker: ticker suspendedDelaysHeap: suspendedDelaysHeap.
	scheduler startTimerEventLoopPriority: Processor activePriority + 1. "So its slightly above normal priority of tests"
	DebugMe ifNil: [ DebugMe := false ]. 
	scheduler debug: DebugMe.

]

{ #category : #running }
DelayBasicSchedulerMicrosecondTickerTest >> tearDown [
"	scheduler debug: false."
	scheduler debug: false.
	super tearDown.
	"scheduler stopTimerEventLoop."

]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testDelayWaitTimeoutCompleted [
	| completed delay status |
	completed := Semaphore new.
	delay := DelayWaitTimeout new setDelay: 100 forSemaphore: completed.
	completed signal.
	
	delay waitOnCompletion: [ status := #completed ] onTimeout: [ status := #timedOut ].
	self assert: status equals: #completed
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testDelayWaitTimeoutTimedOut [
	| completed delay status |
	completed := Semaphore new.
	delay := DelayWaitTimeout new setDelay: 100 forSemaphore: completed.

	delay waitOnCompletion: [ status := #completed ] onTimeout: [ status := #timedOut ].
	self assert: status equals: #timedOut
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testDurationLessThanMaxTicks [
	"In #timingPriorityHandleEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in microseconds"
	| delay |
	delay := Delay new setDelay: 789"ms" forSemaphore: Semaphore new.
	

	scheduler simulate_vmMilliseconds: 100.
	scheduler schedule: delay.
	self assert: ticker vmSimNextWakeupMilliseconds equals: 100 + 789

	
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testDurationMoreThanMaxTicks [
	"In #handleTimerEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in microseconds"
	| delay |
	delay := Delay new setDelay: 1234"ms" forSemaphore: Semaphore new.

	scheduler simulate_vmMilliseconds: 100.
	scheduler schedule: delay.
	self assert: ticker vmSimNextWakeupMilliseconds equals: 100 + 1000
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testForMilliseconds [
	| delay |
	delay := Delay forMilliseconds: 1000.
	
	scheduler simulate_vmMilliseconds: 100.
	scheduler schedule: delay.

	scheduler simulate_vmMilliseconds: 1099.
	self deny: delay isExpired.
	
	scheduler simulate_vmMilliseconds: 1100.
	self assert: delay isExpired.
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testForSeconds [
	| delay |
	delay := Delay forSeconds: 1.
	
	scheduler simulate_vmMilliseconds: 100.
	scheduler schedule: delay.

	scheduler simulate_vmMilliseconds: 1099.
	self deny: delay isExpired.
	
	scheduler simulate_vmMilliseconds: 1100.
	self assert: delay isExpired.
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testHeapBackwards [
	|delay1 delay2 delay3 delay4|
	delay1 := Delay forMilliseconds: 1.
	delay2 := Delay forMilliseconds: 2.
	delay3 := Delay forMilliseconds: 3.
	delay4 := Delay forMilliseconds: 4.
	
	scheduler schedule: delay4.		"activeDelay=delay4, suspendedDelays=()"
	scheduler schedule: delay3.		"activeDelay=delay3, suspendedDelays=(delay4)"
	scheduler schedule: delay2.		"activeDelay=delay2, suspendedDelays=(delay3,delay4)"
	scheduler schedule: delay1. 	"activeDelay=delay1, suspendedDelays=(delay2,delay3,delay4)"

	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay2.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay3.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay4.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: nil.

]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testHeapForwards [
	|delay1 delay2 delay3 delay4|
	delay1 := Delay forMilliseconds: 1.
	delay2 := Delay forMilliseconds: 2.
	delay3 := Delay forMilliseconds: 3.
	delay4 := Delay forMilliseconds: 4.
	
	scheduler schedule: delay1. 	"activeDelay=delay1, suspendedDelays=()"
	scheduler schedule: delay2.		"activeDelay=delay1, suspendedDelays=(delay2)"
	scheduler schedule: delay3.		"activeDelay=delay1, suspendedDelays=(delay2,delay3)"
	scheduler schedule: delay4.		"activeDelay=delay1, suspendedDelays=(delay2,delay3,delay4)"

	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay2.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay3.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: delay4.
	self assert: suspendedDelaysHeap removeFirstOrNil equals: nil.

]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testMillisecondsToGo [
	| delay |
	delay := Delay forMilliseconds: 100.
	
	scheduler simulate_vmMilliseconds: 100.
	scheduler schedule: delay.

	scheduler simulate_vmMilliseconds: 160.
	self assert: delay millisecondsToGo equals: 40.

]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testMillisecondsToGoExpired [
	| delay |
	delay := Delay forMilliseconds: 100.
	
	scheduler simulate_vmMilliseconds: 1.
	scheduler schedule: delay.

	scheduler simulate_vmMilliseconds: 200.
	"self assert: delay millisecondsToGo equals: 0."
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testMultiSchedule [
	| delay anotherDelay |
	"Ensure that scheduling the same delay twice raises an error"
	delay := Delay forSeconds: 1000.
	scheduler schedule: delay.
	self assert: delay beingWaitedOn.
	self should:[ scheduler schedule: delay ] raise: Error.
	
	"Check that after that error, still accepting new items to schedule."
	"Run in a separate process in case #schedule: blocks"
	[	anotherDelay := Delay forMilliseconds: 1000.
		scheduler schedule: anotherDelay.
		] forkAt: Processor activePriority + 1.
	self assert: anotherDelay beingWaitedOn.
	
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testSimpleOneDelay [
	"
	Event loop priority takes So it takes priority over following code"
	|delay|
	delay := Delay new setDelay: 2"ms" forSemaphore: Semaphore new.
	 
	scheduler simulate_vmMilliseconds: 10.
	self deny: delay beingWaitedOn.
	scheduler schedule: delay.
	self assert: delay beingWaitedOn.

	scheduler simulate_vmMilliseconds: 11.
	self deny: delay isExpired. 
	
	scheduler simulate_vmMilliseconds: 12.
	self assert: delay isExpired
	
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testSimpleTwoDelays [
	"Event loop priority takes so it takes priority over following code"
	|delay1 delay2|
	delay1 := Delay new setDelay: 2"ms" forSemaphore: Semaphore new.
	delay2 := Delay new setDelay: 4"ms" forSemaphore: Semaphore new.

	scheduler simulate_vmMilliseconds: 10.	
	self deny: delay1 beingWaitedOn.
	self deny: delay2 beingWaitedOn.
	scheduler schedule: delay1.
	scheduler schedule: delay2.
	self assert: delay1 beingWaitedOn.
	self assert: delay2 beingWaitedOn.
	
	scheduler simulate_vmMilliseconds: 11.
	self deny: delay1 isExpired. 
	self deny: delay2 isExpired. 
	
	scheduler simulate_vmMilliseconds: 12.
	self assert: delay1 isExpired.
	self deny: delay2 isExpired. 
	
	scheduler simulate_vmMilliseconds: 13.
	self deny: delay2 isExpired. 
	
	scheduler simulate_vmMilliseconds: 14.
	self assert: delay2 isExpired.
	
	
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testStartStop [
	|error|
	error := false.
	"scheduler previously started in #setUp"
	[scheduler stopTimerEventLoop] on: Error do: [ error := true ].
	self deny: error
]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testStoppedSchedulerExpiresRemainingDelays [
	"Event loop priority takes So it takes priority over following code"
	|delay1 delay2 delay3|

	delay1 := Delay new setDelay: 9 forSemaphore: Semaphore new.
	delay2 := Delay new setDelay: 99 forSemaphore: Semaphore new.
	delay3 := Delay new setDelay: 999 forSemaphore: Semaphore new.

	scheduler simulate_vmMilliseconds: 10.	
	scheduler schedule: delay1.
	scheduler schedule: delay2.
	scheduler schedule: delay3.
	self assert: delay2 beingWaitedOn.
	self assert: delay1 beingWaitedOn.
	self assert: delay3 beingWaitedOn.

	scheduler simulate_vmMilliseconds: 25.
	self assert: delay1 isExpired.
	self deny: delay2 isExpired. 
	self deny: delay3 isExpired. 

	scheduler stopTimerEventLoop.
	self assert: delay2 isExpired.
	self assert: delay3 isExpired

]

{ #category : #tests }
DelayBasicSchedulerMicrosecondTickerTest >> testSuspendForSnapshot [
	"
	Event loop priority takes So it takes priority over following code"
	|delay|
	delay := Delay new setDelay: 2"ms" forSemaphore: Semaphore new.
	 
	scheduler simulate_vmMilliseconds: 10.
	self deny: delay beingWaitedOn.
	scheduler schedule: delay.
	self assert: delay beingWaitedOn.
	self assert: delay millisecondsToGo equals: 2.
	
	scheduler shutDown. "e.g. by snapshot"

	self assert: delay millisecondsToGo equals: 0.	
	scheduler simulate_vmMilliseconds: 20.
	self assert: delay millisecondsToGo equals: 0.
	self deny: delay isExpired.
	
	scheduler startUp.
	self assert: delay millisecondsToGo equals: 2.
	self deny: delay isExpired.
	scheduler simulate_vmMilliseconds: 23.
	self assert: delay isExpired.
	

]
