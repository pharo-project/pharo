tests
testUnpackSton
	bundle := GTEventPacking ston pack: collector.
	unpackedData := unpacking unpack: bundle data.
	self assert: unpackedData class equals: GTEventAnnouncement.
	self assert: unpackedData unpackedData asArray equals: initialData.