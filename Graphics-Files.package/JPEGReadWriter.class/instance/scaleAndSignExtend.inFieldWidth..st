huffman encoding
scaleAndSignExtend: aNumber inFieldWidth: w

	aNumber < (1 bitShift: (w - 1))
		ifTrue: [^aNumber - (1 bitShift: w) + 1]
		ifFalse: [^aNumber]