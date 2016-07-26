testing
testMcdSerialization
	| stream expected actual |
	expected := self mockDiffyVersion.
	stream := RWBinaryOrTextStream on: String new.
	MCMcdWriter fileOut: expected on: stream.
	actual := MCMcdReader versionFromStream: stream reset.
	self assertVersion: actual matches: expected.