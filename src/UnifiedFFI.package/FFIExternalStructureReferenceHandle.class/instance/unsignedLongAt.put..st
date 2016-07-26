accessing
unsignedLongAt: byteOffset put: value
	^ handle unsignedLongAt: startOffset + byteOffset put: value