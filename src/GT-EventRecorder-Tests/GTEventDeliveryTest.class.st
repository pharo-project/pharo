"
A GTEventDeliveryTest is a test class for testing the behavior of GTEventDelivery
"
Class {
	#name : #GTEventDeliveryTest,
	#superclass : #TestCase,
	#instVars : [
		'delivery',
		'announcer',
		'semaphore'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #converting }
GTEventDeliveryTest >> assertSemaphore [
	self deny: (semaphore waitTimeoutMSecs: 200)
]

{ #category : #testing }
GTEventDeliveryTest >> isGlobalRecorder [
	^ false
]

{ #category : #running }
GTEventDeliveryTest >> setUp [
	super setUp.
	semaphore := Semaphore new.
	delivery := GTEventDelivery new
]

{ #category : #running }
GTEventDeliveryTest >> tearDown [
	
	delivery deactivate.
	super tearDown
]

{ #category : #tests }
GTEventDeliveryTest >> testPrintOn [
	self assert: (String streamContents: [ :stream | delivery printOn: stream ]) notEmpty.
]

{ #category : #tests }
GTEventDeliveryTest >> testPrintOn2 [
	| string |
	delivery recorder: self.
	delivery preSuspendingAction: [ semaphore signal ].
	delivery activate.	
	self assertSemaphore.
	200 milliSeconds wait.
	string := String streamContents: [ :stream | delivery printOn: stream ].
	self assert: string notEmpty.
	self assert: (string includesSubstring: 'next cycle in').
]

{ #category : #tests }
GTEventDeliveryTest >> testPrintProcessName [
	self assert: delivery printProcessName isNotEmpty
]
