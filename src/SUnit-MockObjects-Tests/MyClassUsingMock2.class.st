Class {
	#name : #MyClassUsingMock2,
	#superclass : #TestCase,
	#instVars : [
		'mock'
	],
	#category : #'SUnit-MockObjects-Tests'
}

{ #category : #running }
MyClassUsingMock2 >> setUp [

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

{ #category : #running }
MyClassUsingMock2 >> testMeaningOfLife [

	self assert: (mock meaningOfLife: 22) equals: 42.
	self assert: (mock secondMeaning: 32 and: 64) equals: 84
]

{ #category : #running }
MyClassUsingMock2 >> testMeaningOfLifeDoesNotPassCorrectValue [

	self should: [ self assert: (mock meaningOfLife: 33) equals: 42] raise: TestFailure

]
