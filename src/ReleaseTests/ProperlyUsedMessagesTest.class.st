Class {
	#name : 'ProperlyUsedMessagesTest',
	#superclass : 'TestCase',
	#category : 'ReleaseTests-CleanCode',
	#package : 'ReleaseTests',
	#tag : 'CleanCode'
}

{ #category : 'tests' }
ProperlyUsedMessagesTest >> testSendersOfNotEmpty [
	"Instead of using #notEmpty the method #isNotEmpty should be sent. Check here that 
	 we do not grow with the number of senders."
	
	| violations |
	violations := SystemNavigation default allSendersOf: #notEmpty.
	self assert: violations size < 266
]
