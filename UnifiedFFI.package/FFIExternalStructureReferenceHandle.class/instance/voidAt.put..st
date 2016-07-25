accessing
voidAt: byteOffset put: value
	^ handle voidAt: startOffset + byteOffset put: value