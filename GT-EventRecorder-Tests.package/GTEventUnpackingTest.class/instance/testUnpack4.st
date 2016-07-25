tests
testUnpack4
	bundle := GTEventPacking ston pack: collector.
	unpacking := GTEventUnpacking new.
	unpacking addPacking: GTEventPacking fuel.
	self should: [ unpacking unpack: bundle data ] raise: GTEventUnpackingError 