Class {
	#name : 'GreenTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'tests' }
GreenTest >> testPass [

	self assert: true
]

{ #category : 'tests' }
GreenTest >> testPassAgain [

	self assert: true
]
