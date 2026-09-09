Class {
	#name : 'RedTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'tests' }
RedTest >> testFail [

	self assert: false
]

{ #category : 'tests' }
RedTest >> testPass [

	self assert: true
]

{ #category : 'tests' }
RedTest >> testWithError [

	self assert: nil
]
