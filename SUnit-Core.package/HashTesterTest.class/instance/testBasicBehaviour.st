as yet unclassified
testBasicBehaviour
	self
		should: [(HashTester with: 1)
				resultFor: 100].
	self
		should: [(HashTester with: 'fred')
				resultFor: 100].
	self
		shouldnt: [(HashTester with: BadHasher new)
				resultFor: 100] 