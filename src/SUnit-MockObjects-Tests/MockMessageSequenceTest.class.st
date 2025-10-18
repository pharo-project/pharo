Class {
	#name : 'MockMessageSequenceTest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MockMessageSequenceTest >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife 
		respond: 42. 
	mock 
		on: #meaningOfLife
		respond: 43.
	mock 
		on: #meaningOfLife
		respond: 44
	
]

{ #category : 'running' }
MockMessageSequenceTest >> testThatASequenceOfTheSameMessageReturnsTheCorrespondingValue [
	<ignoreNotImplementedSelectors: #(meaningOfLife secondMeaning)>
	
	self assert: mock meaningOfLife equals: 42.
	self assert: mock meaningOfLife equals: 43.
	self assert: mock meaningOfLife equals: 44.
	
]
