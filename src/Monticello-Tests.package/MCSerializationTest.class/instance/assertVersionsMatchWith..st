asserting
assertVersionsMatchWith: writerClass
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := RWBinaryOrTextStream on: String new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionFromStream: stream reset.
	self assertVersion: actual matches: expected.