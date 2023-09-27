Class {
	#name : 'MyClassUsingMock2Test',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MyClassUsingMock2Test >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife: 
		with: 22
		respond: 42. 
	mock 
		on: #secondMeaning:and:
		with: 32
		with: 64
		respond: 84.
	
]

{ #category : 'running' }
MyClassUsingMock2Test >> testMeaningOfLife [
	<ignoreNotImplementedSelectors: #(meaningOfLife: secondMeaning:and:)>
	self assert: (mock meaningOfLife: 22) equals: 42.
	self assert: (mock secondMeaning: 32 and: 64) equals: 84
]

{ #category : 'running' }
MyClassUsingMock2Test >> testMeaningOfLifeDoesNotPassCorrectValue [

	<ignoreNotImplementedSelectors: #(meaningOfLife:)>
	self should: [ self assert: (mock meaningOfLife: 33) equals: 42 ] raise: TestFailure
]
