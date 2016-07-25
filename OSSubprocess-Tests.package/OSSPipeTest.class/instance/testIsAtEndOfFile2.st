testing
testIsAtEndOfFile2

	| pipe string |
	pipe := self nonBlockingPipe.
	[pipe nextPutAll: 'hello'; flush.
	string := pipe next: 100.
	self assert: string = 'hello'.
	self deny: pipe atEndOfFile.
	pipe closeWriter.
	self deny: pipe atEndOfFile.
	string := pipe next: 100.
	self assert: string = ''.
	self assert: pipe atEndOfFile]
		ensure: [pipe close]
