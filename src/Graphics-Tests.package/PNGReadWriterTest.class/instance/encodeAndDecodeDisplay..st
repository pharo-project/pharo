helpers
encodeAndDecodeDisplay: depth
	| form |
	fileName := 'testDisplay', depth printString,'.png'.
	form := Form extent: (Display extent min: 560@560) depth: depth.
	World fullDrawOn: form getCanvas.
	self encodeAndDecode: form.
	self deleteFile.