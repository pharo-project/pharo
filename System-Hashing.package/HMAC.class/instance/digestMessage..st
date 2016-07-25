accessing
digestMessage: aByteArray
	^ hash hashMessage: (key bitXor: epad), (hash hashMessage: (key bitXor: ipad), aByteArray)