Class {
	#name : 'MockBasicAPITest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MockBasicAPITest >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife 
		respond: 42. 
	mock 
		on: #secondMeaning
		respond: 84.
	
]

{ #category : 'running' }
MockBasicAPITest >> testMeaningOfLife [

	<ignoreNotImplementedSelectors: #(meaningOfLife secondMeaning)>
	
	self assert: mock meaningOfLife equals: 42.
	"We verify that all the messages have been consumed. 
	Since this is not the case we raise an error."
	self should: [ self verify: mock ] raise: TestFailure.
	self assert: mock secondMeaning equals: 84
]

{ #category : 'running' }
MockBasicAPITest >> testMeaningOfLifeIsFirst [

	<ignoreNotImplementedSelectors: #( meaningOfLife secondMeaning )>
	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
	
	"We verify that all the messages have been consumed. 
	Since this is the case we did not get an error."
	self shouldnt: [ self verify: mock ] raise: TestFailure.
]

{ #category : 'running' }
MockBasicAPITest >> testMeaningOfLifeIsSentOnce [
	"A simple mock object can answer a message only once and in the same order."

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	self assert: mock meaningOfLife equals: 42
]

{ #category : 'running' }
MockBasicAPITest >> testMeaningOfLifeIsSentTwice [

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	mock meaningOfLife.
	self should: [ self assert: mock meaningOfLife equals: 42 ] raise: TestFailure
]

{ #category : 'running' }
MockBasicAPITest >> testVerifyChecksThatAllTheMessageGotSent [

	<ignoreNotImplementedSelectors: #( meaningOfLife secondMeaning )>
	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
	self verify: mock
]

{ #category : 'running' }
MockBasicAPITest >> testVerifyFailsWhenMessageShouldBeSent [

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	self assert: mock meaningOfLife equals: 42.
	self should: [ self verify: mock ] raise: TestFailure
]
