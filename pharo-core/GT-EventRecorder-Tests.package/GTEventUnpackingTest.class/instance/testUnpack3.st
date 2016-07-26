tests
testUnpack3
	bundle := GTEventPacking default pack: collector.
	unpacking := GTEventUnpacking new.
	self should: [ unpacking unpack: bundle data ] raise: GTEventInitializationError