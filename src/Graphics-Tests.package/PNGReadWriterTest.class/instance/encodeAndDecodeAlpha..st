helpers
encodeAndDecodeAlpha: original
	fileName := 'testAlpha', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile.