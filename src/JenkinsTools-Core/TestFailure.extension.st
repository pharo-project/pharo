Extension { #name : #TestFailure }

{ #category : #'*JenkinsTools-Core' }
TestFailure >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordFailure: self duringTest: aTestCase
]
