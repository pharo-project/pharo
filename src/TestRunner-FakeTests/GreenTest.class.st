Class {
	#name : 'GreenTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests',
	#package : 'TestRunner-FakeTests'
}

{ #category : 'tests' }
GreenTest >> testPass [

	self assert: true
]

{ #category : 'tests' }
GreenTest >> testPassAgain [

	self assert: true
]
