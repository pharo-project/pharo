asserting
assertNoChange
	| actual |
	actual := MCSnapshotResource takeSnapshot.
	diff := actual patchRelativeToBase: expected.
	self assert: diff isEmpty