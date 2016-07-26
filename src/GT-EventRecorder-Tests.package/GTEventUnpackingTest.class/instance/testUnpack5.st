tests
testUnpack5
	bundle := GTEventPacking fuel pack: collector.
	unpacking := GTEventUnpacking new.
	unpacking addPacking: GTEventPacking ston.
	self should: [ unpacking unpack: bundle data ] raise: GTEventUnpackingError 