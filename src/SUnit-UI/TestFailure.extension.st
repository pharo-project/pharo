Extension { #name : #TestFailure }

{ #category : #'*SUnit-UI' }
TestFailure >> recordResultOf: aTestCase byCommandLine: aCommandLineTestRunner [
	
	aCommandLineTestRunner handleFailure: self of: aTestCase
]
