Extension { #name : 'TestFailure' }

{ #category : '*SUnit-Basic-CLI' }
TestFailure >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordFailure: self duringTest: aTestCase
]
