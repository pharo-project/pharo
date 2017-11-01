"
I am a simple test case to check that HashTester works correctly
"
Class {
	#name : #HashTesterTest,
	#superclass : #TestCase,
	#category : 'SUnit-Core-Utilities'
}

{ #category : #tests }
HashTesterTest >> testBasicBehaviour [
	self
		should: [(HashTester with: 1)
				resultFor: 100].
	self
		should: [(HashTester with: 'fred')
				resultFor: 100].
	self
		shouldnt: [(HashTester with: BadHasher new)
				resultFor: 100] 
]
