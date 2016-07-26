tests
testSelectIsNotShallowCopy
	"self debug: #testSelectIsNotShallowCopy"
	
	| original even |
	original := self collectionClass new.
	original at: #one put: 1.
	original at: #two put: 2.

	even := original select: [:value |value even].
	even at: #two put: 'deux'.

	self assert: (original at: #two) = 2
 	   description: 'modifying a selection should not modify the original'