"
An EpDeferrerTest is a test class for testing the behavior of EpDeferrer
"
Class {
	#name : #OmDeferrerTest,
	#superclass : #TestCase,
	#instVars : [
		'count',
		'deferrer'
	],
	#category : #OmbuTests
}

{ #category : #accessing }
OmDeferrerTest >> count [

	^ count
]

{ #category : #accessing }
OmDeferrerTest >> count: anInteger [ 

	count := anInteger
]

{ #category : #accessing }
OmDeferrerTest >> increase [

	count := count + 1
]

{ #category : #tests }
OmDeferrerTest >> testExecuteOnceAfterScheduling [

	self skipOnPharoCITestingEnvironment.

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 250 milliSeconds.

	deferrer schedule.
	(Delay forMilliseconds: 350) wait.
	
	self assert: self count equals: 1.
]

{ #category : #tests }
OmDeferrerTest >> testExecuteOnceAfterSchedulingMultipleTimes [

	self skipOnPharoCITestingEnvironment.

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 250 milliSeconds.

	3 timesRepeat: [	
		deferrer schedule.
		(Delay forMilliseconds: 50) wait ].
	
	(Delay forMilliseconds: 350) wait.
	
	self assert: self count equals: 1.
]

{ #category : #tests }
OmDeferrerTest >> testExecuteWithoutDelay [

	self skipOnPharoCITestingEnvironment.

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 0 milliSeconds.

	deferrer schedule.

	"Just to document it needs an interruption"
	self assert: self count equals: 0.
	Processor yield.	
	self assert: self count equals: 1.
]
