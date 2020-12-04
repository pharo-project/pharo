Extension { #name : #Exception }

{ #category : #'*JenkinsTools-Core' }
Exception >> recordBackgroundFailureOf: aTestCase inHDTestReport: aHDTestReport [
	
	"Here is a hook for specific reporting of background failures 
	triggered from processes forked during the test.
	
	By default exceptions just use a regular reporting logic"
	
	self recordResultOf: aTestCase inHDTestReport: aHDTestReport
]

{ #category : #'*JenkinsTools-Core' }
Exception >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	"By default exceptions are not accounted in test result and just propogate the signal further.
	But subclasses should decide here how to record them in test result.

	This default logic is in SUnit for a long time 
	but in past it was in senders of this method which handles a predefined set of known exceptions"	
	self pass 
]
