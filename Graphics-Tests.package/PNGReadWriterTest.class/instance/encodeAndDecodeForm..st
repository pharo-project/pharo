helpers
encodeAndDecodeForm: original
	fileName := 'testForm', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile.