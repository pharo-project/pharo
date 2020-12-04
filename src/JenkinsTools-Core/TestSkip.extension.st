Extension { #name : #TestSkip }

{ #category : #'*JenkinsTools-Core' }
TestSkip >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [

	aHDTestReport recordSkip: self duringTest: aTestCase
]
