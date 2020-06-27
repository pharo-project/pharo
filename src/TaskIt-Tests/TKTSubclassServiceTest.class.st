Class {
	#name : #TKTSubclassServiceTest,
	#superclass : #TKTTestCase,
	#instVars : [
		'service'
	],
	#category : #'TaskIt-Tests-Services'
}

{ #category : #running }
TKTSubclassServiceTest >> tearDown [

	(service notNil and: [service isRunning]) ifTrue: [
		(service stop) waitForCompletion: 1 second ].
	super tearDown
]

{ #category : #tests }
TKTSubclassServiceTest >> testServiceExecutesStartUpOnStart [

	service := TKTMockService new.
	service start.
	
	10 milliSeconds wait.
	
	self assert: service isStarted.
]

{ #category : #tests }
TKTSubclassServiceTest >> testServiceExecutesTearDownOnStop [

	service := TKTMockService new.
	service start.
	(service stop) waitForCompletion: 1 second.

	self assert: service isShutDown.
]

{ #category : #tests }
TKTSubclassServiceTest >> testServiceRunsAfterStart [

	service := TKTMockService new.
	service start.
	
	10 milliSeconds wait.
	
	self assert: service hasRun.
]
