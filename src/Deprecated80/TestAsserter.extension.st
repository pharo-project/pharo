Extension { #name : #TestAsserter }

{ #category : #'*Deprecated80' }
TestAsserter class >> suiteClass [
	self
		deprecated: 'Use #classForTestSuite instead.'
		transformWith: '`@receiver suiteClass' -> '`@receiver classForTestSuite'.
	^ self classForTestSuite
]
