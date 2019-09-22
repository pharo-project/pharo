Extension { #name : #TestCase }

{ #category : #'*DrTests' }
TestCase >> drTestsIsAbstract [
	^ false
]

{ #category : #'*DrTests' }
TestCase class >> drTestsIsAbstract [
	^ self isAbstract
]

{ #category : #'*DrTests' }
TestCase >> drTestsSuite [
	^ TestSuite new
		addTest: self;
		yourself
]
