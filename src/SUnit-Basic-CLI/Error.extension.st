Extension { #name : 'Error' }

{ #category : '*SUnit-Basic-CLI' }
Error >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordError: self duringTest: aTestCase
]
