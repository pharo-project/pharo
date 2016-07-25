asserting
assertSnapshotsMatchWith: writerClass
	| readerClass expected stream actual |
	readerClass := writerClass readerClass.
	expected := self mockSnapshot.
	stream := RWBinaryOrTextStream on: String new.
	(writerClass on: stream) writeSnapshot: expected.
	actual := readerClass snapshotFromStream: stream reset.
	self assertSnapshot: actual matches: expected.