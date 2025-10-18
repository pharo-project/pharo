Class {
	#name : 'MockMessageArgumentTest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MockMessageArgumentTest >> setUp [

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
MockMessageArgumentTest >> testMeaningOfLife [
	<ignoreNotImplementedSelectors: #(meaningOfLife: secondMeaning:and:)>
	
	self assert: (mock meaningOfLife: 22) equals: 42.
	self assert: (mock secondMeaning: 32 and: 64) equals: 84.
	self shouldnt: [ self verify: mock ] raise: TestFailure
]

{ #category : 'running' }
MockMessageArgumentTest >> testMeaningOfLifeDoesNotPassCorrectValue [
	<ignoreNotImplementedSelectors: #(meaningOfLife:)>
	
	self should: [ self assert: (mock meaningOfLife: 33) equals: 42 ] raise: TestFailure
]

{ #category : 'running' }
MockMessageArgumentTest >> testMeaningOfLifeFailing [
	<ignoreNotImplementedSelectors: #(meaningOfLife: secondMeaning:and:)>
	
	self assert: (mock meaningOfLife: 22) equals: 42.
	self assert: (mock secondMeaning: 32 and: 65) class equals: MockObject.
	
	self should: [ self assert: (mock secondMeaning: 32 and: 65) equals: 84] raise: TestFailure.
	
]
