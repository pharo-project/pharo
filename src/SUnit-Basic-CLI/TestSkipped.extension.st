Extension { #name : 'TestSkipped' }

{ #category : '*SUnit-Basic-CLI' }
TestSkipped >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [

	aHDTestReport recordSkip: self duringTest: aTestCase
]
