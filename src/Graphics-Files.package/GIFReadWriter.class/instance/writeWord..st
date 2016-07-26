private-encoding
writeWord: aWord
	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord