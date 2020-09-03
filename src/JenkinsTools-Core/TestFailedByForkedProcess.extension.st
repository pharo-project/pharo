Extension { #name : #TestFailedByForkedProcess }

{ #category : #'*JenkinsTools-Core' }
TestFailedByForkedProcess >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	executionEnvironment backgroundFailures do: [ :each | 
		aHDTestReport recordError: each duringTest: aTestCase ]
]
