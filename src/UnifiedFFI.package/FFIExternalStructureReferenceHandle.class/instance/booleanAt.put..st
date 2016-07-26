accessing
booleanAt: byteOffset put: value
	^ handle booleanAt: startOffset + byteOffset put: value