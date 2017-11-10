Extension { #name : #Error }

{ #category : #'*SUnit-Core' }
Error >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addError: aTestCase.
]
