"
It only defines a teardown for ensure that the current pool is properly clean after executing tests.
"
Class {
	#name : #TKTTestCase,
	#superclass : #ParametrizedTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #running }
TKTTestCase >> assertWithGarbageCollect: aBlock [

	| waitingPeriod timesToRun |
	
	waitingPeriod := 100 milliSeconds.
	timesToRun := self defaultTimeLimit / waitingPeriod.

	timesToRun timesRepeat: [ 
		aBlock value ifTrue: [ ^ self ].

		Smalltalk garbageCollect.
		waitingPeriod wait	].

	self fail: 'Timeout waiting for ' , aBlock printString	
]

{ #category : #running }
TKTTestCase >> runCaseManaged [
	^ self runCase
]

{ #category : #running }
TKTTestCase >> setUp [
	super setUp.
	TKTConfiguration
		resetSoleInstance;
		profileNamed: #test.
]

{ #category : #running }
TKTTestCase >> tearDown [

	TKTConfiguration resetSoleInstance.
	super tearDown	
]

{ #category : #running }
TKTTestCase >> waitGarbageCollect [

	5 timesRepeat: [ Smalltalk garbageCollect. 100 milliSecond wait ]
]
