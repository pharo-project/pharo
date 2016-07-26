tests
testShouldKeepHeadReferenceAfterRefresh
	| headReference |
	headReference := log headReference.

	log refresh.

	self assert: log headReference equals: headReference.