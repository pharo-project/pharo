Extension { #name : #TestFailure }

{ #category : #'*JenkinsTools-Core' }
TestFailure >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordError: self duringTest: aTestCase
]
