Extension { #name : #TestFailedByForkedProcess }

{ #category : #'*JenkinsTools-Core' }
TestFailedByForkedProcess >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	executionEnvironment backgroundFailures do: [ :each | 
		each recordResultOf: aTestCase inHDTestReport: aHDTestReport]
]
