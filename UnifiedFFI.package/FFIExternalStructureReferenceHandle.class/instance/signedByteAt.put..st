accessing
signedByteAt: byteOffset put: value
	^ handle signedByteAt: startOffset + byteOffset put: value