tests
testMergeIntoImageWithNoChanges
	| base revB revA1 |

	self change: #a toReturn: 'a'.
	base := self snapshot.
	self change: #b toReturn: 'b'.
	revB := self snapshot.
	
	self load: base.
	self change: #a toReturn: 'a1'.
	revA1 := self snapshot.

	self change: #a toReturn: 'a'.
	self snapshot.
	self merge: revB.

	self assert: (workingCopy ancestors size = 2)
	