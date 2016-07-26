helpers
encodeAndDecodeColor: aColor depth: aDepth
	| aForm |
	fileName := 'testColor', aColor name, aDepth printString,'.png'.
	aForm := Form extent: 32@32 depth: aDepth.
	aForm fillColor: aColor.
	self encodeAndDecode: aForm.
	self deleteFile.
