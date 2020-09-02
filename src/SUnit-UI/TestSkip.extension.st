Extension { #name : #TestSkip }

{ #category : #'*SUnit-UI' }
TestSkip >> recordResultOf: aTestCase byCommandLine: aCommandLineTestRunner [
	
	aCommandLineTestRunner handleSkip: self of: aTestCase
]
