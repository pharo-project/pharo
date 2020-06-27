Class {
	#name : #TKTServiceTest,
	#superclass : #TKTTestCase,
	#instVars : [
		'service'
	],
	#category : #'TaskIt-Tests-Services'
}

{ #category : #running }
TKTServiceTest >> tearDown [

	(service notNil and: [service isRunning]) ifTrue: [
		(service stop) waitForCompletion: 1 second ].
	super tearDown
]

{ #category : #tests }
TKTServiceTest >> testPrioritySetAfterStart [
	|newPriority|
	service := TKTParameterizableService new.
	service name: 'test service'.
	newPriority := 32.
	self deny: service priority equals: newPriority.
	service start.
	service priority: newPriority.
	self assert: service priority equals: newPriority.
	self assert: service process priority equals: newPriority.

]

{ #category : #tests }
TKTServiceTest >> testPrioritySetBeforeStart [
	|initialPriority|
	service := TKTParameterizableService new.
	service name: 'test service'.
	initialPriority := 35.
	service priority: initialPriority.
	self assert: service priority equals: initialPriority.
	service start.
	self assert: service process priority equals: initialPriority.

]

{ #category : #tests }
TKTServiceTest >> testServiceIsKilledAfterStop [

	service := TKTParameterizableService new.
	service name: 'test service'.
	service start.
	(service stop) waitForCompletion: 2 second.
	
	self deny: service isRunning.
]

{ #category : #tests }
TKTServiceTest >> testServiceShouldHaveAName [

	self should: [TKTService new start] raise: Error
]
