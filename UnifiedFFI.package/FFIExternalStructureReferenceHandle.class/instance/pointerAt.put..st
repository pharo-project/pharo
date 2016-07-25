accessing
pointerAt: byteOffset put: value
	^ handle pointerAt: startOffset + byteOffset put: value