Class {
	#name : 'MCGitBasedNetworkRepositoryTest',
	#superclass : 'TestCase',
	#category : 'Monticello-Tests-Metacello',
	#package : 'Monticello-Tests',
	#tag : 'Metacello'
}

{ #category : 'tests' }
MCGitBasedNetworkRepositoryTest >> testTempFileFor [
	"This should be enough to ensure that https://github.com/dalehenrich/metacello-work/issues/405 doesn't happen again"

	| tempFilePath |
	tempFilePath := MCGitBasedNetworkRepository tempFileFor: 'xxx' suffix: '.test'
]
