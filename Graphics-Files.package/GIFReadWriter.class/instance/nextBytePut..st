private-packing
nextBytePut: aByte
	bufStream nextPut: aByte.
	bufStream size >= 254 ifTrue: [self flushBuffer]