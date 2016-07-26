testing
testSimple
	| input reader |
	input := 'Foo', String crlf, 'Bar', String crlf.
	reader := ZnLineReader on: input readStream.
	self assert: reader nextLine = 'Foo'.
	self assert: reader nextLine = 'Bar'.
	self assert: reader nextLine isEmpty