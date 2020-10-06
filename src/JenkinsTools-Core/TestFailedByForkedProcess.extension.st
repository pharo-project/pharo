Extension { #name : #TestFailedByForkedProcess }

{ #category : #'*JenkinsTools-Core' }
TestFailedByForkedProcess >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	executionEnvironment backgroundFailures do: [ :each | 
		each recordBackgroundFailureOf: aTestCase inHDTestReport: aHDTestReport ]
]
