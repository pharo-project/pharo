testing
testReading
	| mimeType |
	mimeType := ZnMimeType fromString: 'text/plain; charset=utf-8'.
	self assert: (mimeType main = 'text').
	self assert: (mimeType sub = 'plain').
	self assert: (mimeType charSet = 'utf-8').
	self assert: mimeType isCharSetUTF8.
	self assert: mimeType isBinary not