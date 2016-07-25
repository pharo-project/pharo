accessing
signedLongAt: byteOffset put: value
	^ handle signedLongAt: startOffset + byteOffset put: value