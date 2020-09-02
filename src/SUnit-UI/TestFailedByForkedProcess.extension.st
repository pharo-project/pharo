Extension { #name : #TestFailedByForkedProcess }

{ #category : #'*SUnit-UI' }
TestFailedByForkedProcess >> recordResultOf: aTestCase byCommandLine: aCommandLineTestRunner [
	
	executionEnvironment failures do: [ :each |
		aCommandLineTestRunner handleFailure: each of: aTestCase ]
]
