tests
testMergeIntoUnmodifiedImage
	| base revA |

	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revA := self snapshot.
	
	self load: base.

	self merge: revA.

	self assert: (workingCopy ancestors size = 1)
	