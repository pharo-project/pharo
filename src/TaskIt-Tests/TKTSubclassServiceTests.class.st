Class {
	#name : #TKTSubclassServiceTests,
	#superclass : #TKTTestCase,
	#instVars : [
		'service'
	],
	#category : #'TaskIt-Tests-Services'
}

{ #category : #running }
TKTSubclassServiceTests >> tearDown [

	super tearDown.
	(service notNil and: [service isRunning]) ifTrue: [
		(service stop) waitForCompletion: 1 second ]
]

{ #category : #tests }
TKTSubclassServiceTests >> testServiceExecutesStartUpOnStart [

	service := TKTMockService new.
	service start.
	
	10 milliSeconds wait.
	
	self assert: service isStarted.
]

{ #category : #tests }
TKTSubclassServiceTests >> testServiceExecutesTearDownOnStop [

	service := TKTMockService new.
	service start.
	(service stop) waitForCompletion: 1 second.

	self assert: service isShutDown.
]

{ #category : #tests }
TKTSubclassServiceTests >> testServiceRunsAfterStart [

	service := TKTMockService new.
	service start.
	
	10 milliSeconds wait.
	
	self assert: service hasRun.
]
