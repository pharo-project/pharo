tests
testConvertingNewLines
	| input result output |
	input := '''line ending with CR', String cr, 
		'line ending with LF', String lf, 
		'line ending with CRLF', String crlf, ''''.
	output := 'line ending with CR', String crlf, 
		'line ending with LF', String crlf, 
		'line ending with CRLF', String crlf.
	result := (STON reader on: input readStream) newLine: String crlf; convertNewLines: true; next.
	self assert: result equals: output.
	output := 'line ending with CR', String cr, 
		'line ending with LF', String cr, 
		'line ending with CRLF', String cr.
	result := (STON reader on: input readStream) newLine: String cr; convertNewLines: true; next.
	self assert: result equals: output