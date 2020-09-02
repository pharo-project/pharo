Extension { #name : #Halt }

{ #category : #'*SUnit-UI' }
Halt >> recordResultOf: aTestCase byCommandLine: aCommandLineTestRunner [
	
	aCommandLineTestRunner handleFailure: self of: aTestCase
]
