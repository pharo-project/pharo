testing
testReadingMultilineMultiValue
	| string headers |
	string := 'Content-Type: text/plain', String crlf, 
		'Long: foo', String crlf, 
		' bar', String crlf, 
		String tab, 'baz', String crlf, 
		'Content-Length: 128', String crlf,
		'Long: more', String crlf.
	headers := ZnHeaders readFrom: string asByteArray readStream.
	self assert: (headers at: #long) equals: #( 'foo bar	baz' 'more').