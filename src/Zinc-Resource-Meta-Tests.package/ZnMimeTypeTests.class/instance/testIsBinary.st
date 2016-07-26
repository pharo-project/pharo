testing
testIsBinary
	self assert: ZnMimeType applicationOctetStream isBinary.
	self assert: ZnMimeType imagePng isBinary.
	self deny: ZnMimeType textPlain isBinary.
	self deny: ZnMimeType textHtml isBinary.
	self deny: ZnMimeType textJavascript isBinary.
	self deny: ZnMimeType applicationJson isBinary.
	self someNonBinaryMimeTypes do: [ :each |
		self deny: each asZnMimeType isBinary ]