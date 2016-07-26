testing
testAsMimeType
	self assert: 'text/plain;charset=utf-8' asZnMimeType equals: ZnMimeType textPlain.
	self assert: ZnMimeType applicationOctetStream asZnMimeType equals: ZnMimeType applicationOctetStream