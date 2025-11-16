Extension { #name : 'Halt' }

{ #category : '*SUnit-Basic-CLI' }
Halt >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordError: self duringTest: aTestCase
]
