Extension { #name : #Halt }

{ #category : #'*JenkinsTools-Core' }
Halt >> recordResultOf: aTestCase inHDTestReport: aHDTestReport [
	
	aHDTestReport recordError: self duringTest: aTestCase
]
