tests
testStoreAndLoad
	| node node2 |
	node := self saveSnapshot1.
	node2 := self saveSnapshot2.
	self assert: (self snapshotAt: node) = self snapshot1.
	self assert: (self snapshotAt: node2) = self snapshot2.