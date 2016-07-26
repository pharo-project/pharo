testing
testReadingMultilineNonBinary
	| string headers |
	string := 'Foo: 1', String crlf, 
		' 2', String crlf, 
		String tab, '3', String crlf, 
		'Bar: EOF', String crlf.
	headers := ZnHeaders readFrom: string readStream.
	self assert: (headers at: 'Foo') equals: '1 2', String tab, '3'.
	self assert: (headers at: 'Bar') equals: 'EOF'