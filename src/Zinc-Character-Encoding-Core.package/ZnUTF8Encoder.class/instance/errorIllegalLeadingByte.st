error handling
errorIllegalLeadingByte
	^ self error: 'Illegal leading byte for utf-8 encoding'