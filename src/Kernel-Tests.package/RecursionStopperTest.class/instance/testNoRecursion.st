tests
testNoRecursion

	RecursionStopper during: [ value := value + 1 ].

	self assert: value equals: 1