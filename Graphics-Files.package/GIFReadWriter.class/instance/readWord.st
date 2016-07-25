private-decoding
readWord
	^self next + (self next bitShift: 8)