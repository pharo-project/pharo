testing
testWriting
	| mimeType |
	(mimeType := ZnMimeType main: 'text' sub: 'plain')
		charSet: 'utf-8'.
	self assert: mimeType printString = 'text/plain;charset=utf-8'.
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not