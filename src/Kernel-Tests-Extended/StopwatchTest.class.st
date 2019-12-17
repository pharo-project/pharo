"
SUnit tests for the stopwatch 
"
Class {
	#name : #StopwatchTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'aStopwatch',
		'aDelay'
	],
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #testing }
StopwatchTest class >> isUnitTest [
	^false
]

{ #category : #coverage }
StopwatchTest >> classToBeTested [

	^ Stopwatch


]

{ #category : #coverage }
StopwatchTest >> selectorsToBeIgnored [

	| private | 
	private := #( #printOn: #state: ).

	^ super selectorsToBeIgnored, private

]

{ #category : #running }
StopwatchTest >> setUp [
	super setUp.
	aStopwatch := Stopwatch new.
	aDelay := Delay forMilliseconds: 1.
]

{ #category : #tests }
StopwatchTest >> testChangingStatus [
	aStopwatch activate.
	self assert: aStopwatch isActive.
	self assert: aStopwatch timespans size equals: 1.
	aStopwatch suspend.
	self assert: aStopwatch isSuspended.
	self assert: aStopwatch timespans size equals: 1.
	aStopwatch activate.
	aStopwatch reActivate.
	self assert: aStopwatch isActive.
	self assert: aStopwatch timespans size equals: 3.
	aStopwatch reset.
	self assert: aStopwatch isSuspended.
	self assert: aStopwatch timespans size equals: 0
]

{ #category : #tests }
StopwatchTest >> testInitialStatus [
	self assert: aStopwatch isSuspended.
	self deny: aStopwatch isActive.
	self assert: aStopwatch duration equals: 0 seconds
]

{ #category : #tests }
StopwatchTest >> testMultipleTimings [
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	self assert: aStopwatch timespans size equals: 2.
	self assert: aStopwatch timespans first asDateAndTime <= aStopwatch timespans last asDateAndTime
]

{ #category : #tests }
StopwatchTest >> testNew [
	| sw |
	sw := Stopwatch new.

	self
		assert: sw isSuspended;
		assert: sw state equals: #suspended;
		deny: sw isActive;
		assertEmpty: sw timespans
]

{ #category : #tests }
StopwatchTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aStopwatch printOn: str ]) equals: 'a Stopwatch(suspended:0:00:00:00)'
]

{ #category : #tests }
StopwatchTest >> testReActivate [

	| sw |
	sw := Stopwatch new.
	sw 
		activate;
		suspend;
		reActivate.
	
	self 
		assert: (sw isActive).

]

{ #category : #tests }
StopwatchTest >> testReset [
	| sw |
	sw := Stopwatch new.
	sw activate.

	sw reset.
	self
		assert: sw isSuspended;
		assertEmpty: sw timespans
]

{ #category : #tests }
StopwatchTest >> testSingleTiming [
	| timeBefore |
	timeBefore := DateAndTime now.
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	self assert: aStopwatch timespans size equals: 1.
	self assert: aStopwatch timespans first asDateAndTime >= timeBefore.
	self assert: aStopwatch timespans first asDateAndTime <= aStopwatch end
]

{ #category : #tests }
StopwatchTest >> testStartStop [
	| sw t1 t2 t3 t4 |
	sw := Stopwatch new.
	t1 := DateAndTime now.
	(Delay forMilliseconds: 10) wait.
	sw
		activate;
		activate.
	(Delay forMilliseconds: 10) wait.
	t2 := DateAndTime now.

	self
		deny: sw isSuspended;
		assert: sw isActive;
		assert: sw timespans size equals: 1;
		assert: t1 <= sw start;
		assert: sw start <= t2.

	(Delay forMilliseconds: 10) wait.
	t3 := DateAndTime now.
	(Delay forMilliseconds: 10) wait.
	sw
		suspend;
		suspend.
	(Delay forMilliseconds: 10) wait.
	t4 := DateAndTime now.

	self
		assert: sw isSuspended;
		deny: sw isActive;
		assert: sw timespans size equals: 1;
		assert: (sw end between: t3 and: t4);
		assert: t3 <= sw end;
		assert: sw end <= t4
]
