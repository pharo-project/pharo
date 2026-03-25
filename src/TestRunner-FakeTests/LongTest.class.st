Class {
	#name : 'LongTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-FakeTests',
	#package : 'TestRunner-FakeTests'
}

{ #category : 'tests' }
LongTest >> testIsTakingTooLong [

	(Delay forSeconds: 3) wait.
	self 
		ignore;
	 	assert: true
]
