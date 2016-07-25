private-bits access
flushBits
	remainBitCount = 0 ifFalse: 
		[ self nextBytePut: bufByte.
		remainBitCount := 0 ].
	self flushBuffer