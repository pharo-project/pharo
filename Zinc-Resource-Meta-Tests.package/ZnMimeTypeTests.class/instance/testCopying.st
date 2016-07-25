testing
testCopying
	| mimeType1 mimeType2 |
	mimeType1 := ZnMimeType textPlain.
	mimeType2 := ZnMimeType textPlain.
	self assert: mimeType1 = mimeType2.
	self assert: mimeType1 parameters = mimeType2 parameters.
	mimeType1 charSet: 'utf-8'.
	self assert: mimeType1 charSet = 'utf-8'.
	mimeType2 charSet: 'latin1'.
	self assert: mimeType2 charSet = 'latin1'.
	self assert: (mimeType1 matches: mimeType2).
	self deny: mimeType1 parameters = mimeType2 parameters.
	self deny: mimeType1 charSet = mimeType2 charSet.
	