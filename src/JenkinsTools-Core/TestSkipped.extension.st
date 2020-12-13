Extension { #name : #TestSkipped }

{ #category : #'*JenkinsTools-Core' }
TestSkipped >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [

	aHDTestReport recordSkip: self duringTest: aTestCase
]
