tests
testShouldKeepHeadReferenceAfterReOpen
	| anotherLog |

	anotherLog := EpLog newWithStore: log store copy refresh.

	self assert: anotherLog headReference equals: log headReference.