Extension { #name : #SkippingTest }

{ #category : #'*JenkinsTools-Core' }
SkippingTest >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [

	aHDTestReport recordSkip: self duringTest: aTestCase
]
