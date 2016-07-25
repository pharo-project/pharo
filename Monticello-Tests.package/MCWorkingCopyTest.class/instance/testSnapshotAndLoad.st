tests
testSnapshotAndLoad
	| base inst |
	inst := self mockInstanceA.
	base :=  self snapshot.
	self change: #one toReturn: 2.
	self assert: inst one = 2.
	self load: base.
	self assert: inst one = 1.