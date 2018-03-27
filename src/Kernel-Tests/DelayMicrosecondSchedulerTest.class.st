"
SUnit tests for class DelayMicrosecondScheduler


"
Class {
	#name : #DelayMicrosecondSchedulerTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #support }
DelayMicrosecondSchedulerTest >> busyWaitMilliseconds: milliseconds [
	| waitUntil |
	waitUntil := Time millisecondClockValue + milliseconds.
	[ Time millisecondClockValue < waitUntil ] whileTrue.

]

{ #category : #settings }
DelayMicrosecondSchedulerTest >> delaySchedulerClass [
	^DelayMicrosecondScheduler
]

{ #category : #running }
DelayMicrosecondSchedulerTest >> tearDown [ 	
	"Trigger system timingSempaphore, in case it was replaced"
	Delay testCaseSupportTimingSemaphore signal.
	super tearDown
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testDurationLessThanMaxTicks [
	"In #handleTimerEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in microseconds"
	| duration nowTick nextTick expectedTick scheduler |
	duration := 789 milliSeconds.  "simply less than maxTicks"
	nowTick := 100 milliSeconds. 
	expectedTick := nowTick + duration.

	scheduler := self delaySchedulerClass new.
	scheduler schedule: duration asDelay. 
	nextTick := scheduler handleTimerEvent: nowTick asMicroseconds.

	self assert: nextTick 
			equals: expectedTick asMicroseconds.		
	self assert: scheduler testCaseSupportActiveDelay millisecondDelayDuration 
			equals: duration asMilliSeconds.
	
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testDurationMoreThanMaxTicks [
	"In #handleTimerEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in microseconds"
	| duration maxTicks  nowTick nextTick expectedTick scheduler |
	duration := 1234 milliSeconds. "simply more than maxTicks"
	maxTicks := 1000 milliSeconds. 
	nowTick := 100 milliSeconds.
	expectedTick := nowTick + maxTicks.

	scheduler := self delaySchedulerClass new.
	scheduler schedule: duration asDelay. 
	nextTick := scheduler handleTimerEvent: nowTick asMicroseconds.

	self assert: nextTick 
			equals: expectedTick asMicroseconds.		
	self assert: scheduler testCaseSupportActiveDelay millisecondDelayDuration 
			equals: duration asMilliSeconds.
	
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testForMilliseconds [
	| scheduler delay preTick postTick actualDuration |
	scheduler := self delaySchedulerClass new.
	delay := Delay forMilliseconds: 1000.
	scheduler schedule: delay.
	
	preTick := Time millisecondClockValue.
	[ 	scheduler handleTimerEvent: Time microsecondClockValue.
		Processor yield.
		delay delaySemaphore isSignaled
	] whileFalse.
	postTick := Time millisecondClockValue.
	
	actualDuration := postTick - preTick.
	self assert: actualDuration >= 950.
	self assert: actualDuration <= 1050.
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testForSeconds [
	| scheduler delay preTick postTick actualDuration |
	scheduler := self delaySchedulerClass new.
	delay := Delay forSeconds: 1.
	scheduler schedule: delay.
	
	preTick := Time millisecondClockValue.
	[ 	scheduler handleTimerEvent: Time microsecondClockValue.
		Processor yield.
		delay delaySemaphore isSignaled
	] whileFalse.
	postTick := Time millisecondClockValue.
	
	actualDuration := postTick - preTick.
	self assert: actualDuration >= 950.
	self assert: actualDuration <= 1050.
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testMillisecondsToGo [
	| delay scheduler millisecondsToGo |
	scheduler := self delaySchedulerClass new.
	delay := Delay forMilliseconds: 100.
	scheduler schedule: delay.
	scheduler handleTimerEvent: Time microsecondClockValue.
	self busyWaitMilliseconds: 50.
	millisecondsToGo := delay millisecondsToGo.
	self assert: millisecondsToGo >= 40.
	self assert: millisecondsToGo <= 60.
]

{ #category : #tests }
DelayMicrosecondSchedulerTest >> testTestInterferenceWithSystemTimingSemaphore [
	| scheduler delay higherPriorityProcess delayExpired |
	
	self skip. 
	
	"Just clarifying how the primitive works.  If our test case methods schedule any delays, 
	the consenquent call to #primSignal:atMilliseconds: replaces the system timingSemaphore, 
	preventing it being triggered to expire normal system delays, which would lead totestScheduler 
	system lockup.  This test indicates that timingSemaphore must be triggered to reset in #tearDown."
	
	scheduler := self delaySchedulerClass new.
	
	"FIRST SHOW HOW IT WORKS PROPERLY"
	delay := Delay forMilliseconds: 100.
	higherPriorityProcess := 
		[	delayExpired := false. 
			delay wait.
			delayExpired := true.
			] newProcess
				priority: Processor activeProcess priority + 1 ;
				name: 'DelaySchedulerTest'.		
	higherPriorityProcess resume. "The higher priority process sets delayExpired to false, then waits."
	self deny: delayExpired.		
	self busyWaitMilliseconds: 200.  
	self assert: delayExpired.		"After the wait, delayExpired was set to true"
	
	"THEN SHOW HOW IT BREAKS"			
	[	
		delay := Delay forMilliseconds: 100.
		higherPriorityProcess := 
			[	delayExpired := false. 
				delay wait.
				delayExpired := true.
				] newProcess
					priority: Processor activeProcess priority + 1 ;
					name: 'DelaySchedulerTest'.		
		higherPriorityProcess resume. "The higher priority process sets delayExpired to false, then waits."
		self deny: delayExpired.	
		scheduler primSignal: Semaphore new atUTCMicroseconds: 1. "But the timingSemaphore is replaced."		
		self busyWaitMilliseconds: 200.  
		self deny: delayExpired.		"So the system timer event loop is never signalled to expire the delay"
	] 
	ensure: 
	[	"Clean up and trigger original system timingSemaphore"
		Delay testCaseSupportTimingSemaphore signal. "restart system timer event loop to expire the delay."
		self assert: delayExpired. 
		higherPriorityProcess terminate.
	].


]
