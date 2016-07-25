as yet unclassified
assertNoChange
	| actual |
	actual := MCSnapshotResource takeSnapshot.
	diff := actual patchRelativeToBase: expected snapshot.
	self assert: diff isEmpty