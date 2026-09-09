Class {
	#name : 'YellowTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'tests' }
YellowTest >> testFail [

	self assert: false
]

{ #category : 'tests' }
YellowTest >> testPass [
	self assert: true.
]
