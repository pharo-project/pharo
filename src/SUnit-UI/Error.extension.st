Extension { #name : #Error }

{ #category : #'*SUnit-UI' }
Error >> recordResultOf: aTestCase byCommandLine: aCommandLineTestRunner [
	
	aCommandLineTestRunner handleFailure: self of: aTestCase
]
