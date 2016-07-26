helpers
encodeAndDecodeWithError: aStream
	self should:[self encodeAndDecodeStream: aStream] raise: Error