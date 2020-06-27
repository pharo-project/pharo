Class {
	#name : #TKTParameterizedServiceTest,
	#superclass : #TKTTestCase,
	#instVars : [
		'service'
	],
	#category : #'TaskIt-Tests-Services'
}

{ #category : #running }
TKTParameterizedServiceTest >> tearDown [

	(service notNil and: [service isRunning]) ifTrue: [
		(service stop) waitForCompletion: 1 second ].
	super tearDown	
]

{ #category : #tests }
TKTParameterizedServiceTest >> testServiceExecutesStartUpOnStart [

	| started |
	started := false.
	service := TKTParameterizableService new.
	service name: 'test service'.
	service onSetUpDo: [ started := true ].
	service start.
	
	100 milliSeconds wait.
	
	self assert: started.
]

{ #category : #tests }
TKTParameterizedServiceTest >> testServiceExecutesTearDownOnStop [

	| stopped |
	stopped := false.
	service := TKTParameterizableService new.
	service name: 'test service'.
	service onTearDownDo: [ stopped := true ].
	service start.
	(service stop) waitForCompletion: 2 second.

	self assert: stopped.
]

{ #category : #tests }
TKTParameterizedServiceTest >> testServiceRunsAfterStart [

	| run |
	run := false.
	service := TKTParameterizableService new.
	service name: 'test service'.
	service step: [ run := true ].
	service start.
	
	10 milliSeconds wait.
	
	self assert: run.
]
