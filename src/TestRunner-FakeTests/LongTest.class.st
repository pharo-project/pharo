Class {
	#name : 'LongTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'tests' }
LongTest >> testIsTakingTooLong [

	(Delay forSeconds: 3) wait.
	 self assert: true
]
