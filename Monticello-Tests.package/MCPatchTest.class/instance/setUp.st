running
setUp
	|rev1 rev2|
	rev1 :=  MCSnapshotResource takeSnapshot.
	self change: #one toReturn: 2.
	rev2 :=  MCSnapshotResource takeSnapshot.
	patch := rev2 patchRelativeToBase: rev1.
	self change: #one toReturn: 1.