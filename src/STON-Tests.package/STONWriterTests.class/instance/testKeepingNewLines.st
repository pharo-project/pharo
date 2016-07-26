tests
testKeepingNewLines
	| input result output |
	input := 'line ending with CR', String cr, 
		'line ending with LF', String lf, 
		'line ending with CRLF', String crlf.
	output := '''line ending with CR', String crlf, 
		'line ending with LF', String crlf, 
		'line ending with CRLF', String crlf, ''''.
	result := String streamContents: [ :out |
		(STON writer on: out) newLine: String crlf; keepNewLines: true; nextPut: input ].
	self assert: result equals: output.
	output := '''line ending with CR', String cr, 
		'line ending with LF', String cr, 
		'line ending with CRLF', String cr, ''''.
	result := String streamContents: [ :out |
		(STON writer on: out) newLine: String cr; keepNewLines: true; nextPut: input ].
	self assert: result equals: output