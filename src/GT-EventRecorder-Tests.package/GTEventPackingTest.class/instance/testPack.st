tests-packing
testPack
	self assertBundle.
	announcement := packing unpack: bundle data.
	self assertUnpackedData