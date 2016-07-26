testing
testReading
	| contents entity |
	contents := 'This is a test'.
	entity := ZnEntity 
				readFrom: contents asByteArray readStream 
				usingType: ZnMimeType textPlain 
				andLength: contents size.
	self assert: entity contents isString.
	self assert: entity string = contents.
	self assert: entity contentLength = contents size.
	self assert: entity contentType = ZnMimeType textPlain 