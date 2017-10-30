error handling
errorIllegalContinuationByte
	^ self error: 'Illegal continuation byte for utf-8 encoding'