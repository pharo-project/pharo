accessing
signedShortAt: byteOffset put: value
	^ handle signedShortAt: startOffset + byteOffset put: value