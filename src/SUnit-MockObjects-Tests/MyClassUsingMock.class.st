Class {
	#name : #MyClassUsingMock,
	#superclass : #TestCase,
	#instVars : [
		'mock'
	],
	#category : #'SUnit-MockObjects-Tests'
}

{ #category : #running }
MyClassUsingMock >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife 
		respond: 42. 
	mock 
		on: #secondMeaning
		respond: 84.
	
]

{ #category : #running }
MyClassUsingMock >> testMeaningOfLife [

	self assert: mock meaningOfLife equals: 42.
	self should: [ self verify: mock ] raise: TestFailure.
	self assert: mock secondMeaning equals: 84
]

{ #category : #running }
MyClassUsingMock >> testMeaningOfLifeIsFirst [

	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
		
]

{ #category : #running }
MyClassUsingMock >> testMeaningOfLifeIsSentOnce [
	"A simple mock object can answer a message only once and in the same order."
	
	self assert: mock meaningOfLife equals: 42.

	
]

{ #category : #running }
MyClassUsingMock >> testMeaningOfLifeIsSentTwice [
	
	mock meaningOfLife.
	self should: [self assert: mock meaningOfLife equals: 42] raise: TestFailure.

	
]

{ #category : #running }
MyClassUsingMock >> testVerifyChecksThatAllTheMessageGotSent [

	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
	self verify: mock
		
]

{ #category : #running }
MyClassUsingMock >> testVerifyFailsWhenMessageShouldBeSent [

	self assert: mock meaningOfLife equals: 42.
	self should: [ self verify: mock ] raise: TestFailure.
		
]
