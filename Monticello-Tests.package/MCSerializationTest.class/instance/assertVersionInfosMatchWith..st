asserting
assertVersionInfosMatchWith: writerClass
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := RWBinaryOrTextStream on: String new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionInfoFromStream: stream reset.
	self assert: actual = expected info.