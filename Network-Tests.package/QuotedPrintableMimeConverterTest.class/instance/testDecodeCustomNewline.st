testing
testDecodeCustomNewline
	| decoder output |
	decoder := coder new.
	output := String new writeStream.
	decoder 
		newline: String lf;
		mimeStream: 'foo=0D=0Abar' readStream;
		dataStream: output;
		mimeDecode.
	self 
		assert: output contents
		equals: 'foo', String lf, 'bar'
	