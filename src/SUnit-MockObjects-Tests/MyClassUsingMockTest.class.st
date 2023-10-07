Class {
	#name : 'MyClassUsingMockTest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MyClassUsingMockTest >> setUp [

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
MyClassUsingMockTest >> testMeaningOfLife [

	<ignoreNotImplementedSelectors: #(meaningOfLife secondMeaning)>
	self assert: mock meaningOfLife equals: 42.
	self should: [ self verify: mock ] raise: TestFailure.
	self assert: mock secondMeaning equals: 84
]

{ #category : 'running' }
MyClassUsingMockTest >> testMeaningOfLifeIsFirst [

	<ignoreNotImplementedSelectors: #( meaningOfLife secondMeaning )>
	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84
]

{ #category : 'running' }
MyClassUsingMockTest >> testMeaningOfLifeIsSentOnce [
	"A simple mock object can answer a message only once and in the same order."

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	self assert: mock meaningOfLife equals: 42
]

{ #category : 'running' }
MyClassUsingMockTest >> testMeaningOfLifeIsSentTwice [

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	mock meaningOfLife.
	self should: [ self assert: mock meaningOfLife equals: 42 ] raise: TestFailure
]

{ #category : 'running' }
MyClassUsingMockTest >> testVerifyChecksThatAllTheMessageGotSent [

	<ignoreNotImplementedSelectors: #( meaningOfLife secondMeaning )>
	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84.
	self verify: mock
]

{ #category : 'running' }
MyClassUsingMockTest >> testVerifyFailsWhenMessageShouldBeSent [

	<ignoreNotImplementedSelectors: #( meaningOfLife )>
	self assert: mock meaningOfLife equals: 42.
	self should: [ self verify: mock ] raise: TestFailure
]
