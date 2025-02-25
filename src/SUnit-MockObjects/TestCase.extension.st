Extension { #name : 'TestCase' }

{ #category : '*SUnit-MockObjects' }
TestCase >> verify: aMockObject [
	"Verify that all the messages have been consumed and that the test did not failed."
	
	^ aMockObject verifyIn: self
]
