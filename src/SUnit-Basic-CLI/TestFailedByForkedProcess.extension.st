Extension { #name : 'TestFailedByForkedProcess' }

{ #category : '*SUnit-Basic-CLI' }
TestFailedByForkedProcess >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	executionEnvironment backgroundFailures do: [ :each | 
		each recordBackgroundFailureOf: aTestCase inHDTestReport: aHDTestReport ]
]
