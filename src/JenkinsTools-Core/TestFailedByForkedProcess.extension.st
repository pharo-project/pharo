Extension { #name : #TestFailedByForkedProcess }

{ #category : #'*JenkinsTools-Core' }
TestFailedByForkedProcess >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	executionEnvironment failures do: [ :each | 
		aHDTestReport recordError: each duringTest: aTestCase ]
]
