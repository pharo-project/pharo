Extension { #name : #Error }

{ #category : #'*JenkinsTools-Core' }
Error >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordError: self duringTest: aTestCase
]
