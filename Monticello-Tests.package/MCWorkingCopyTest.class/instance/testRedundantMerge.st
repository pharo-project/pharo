tests
testRedundantMerge
	| base |
	base := self snapshot.
	self merge: base.
	self merge: base