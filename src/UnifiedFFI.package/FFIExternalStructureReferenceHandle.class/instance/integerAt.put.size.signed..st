accessing
integerAt: byteOffset put: value size: nBytes signed: aBoolean
	^ handle integerAt: startOffset + byteOffset put: value size: nBytes signed: aBoolean