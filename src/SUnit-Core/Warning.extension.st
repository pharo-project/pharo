Extension { #name : #Warning }

{ #category : #'*SUnit-Core' }
Warning >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addFailure: aTestCase.
]
