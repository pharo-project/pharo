testing
testNonBlockingPeek

	| pipe |
	pipe := self nonBlockingPipe.
	pipe nextPutAll: 'abc'; flush.
	self assert: (pipe peek == $a).
	self assert: (pipe next == $a).
	self assert: (pipe peek == $b).
	self assert: (pipe next == $b).
	self assert: (pipe peek == $c).
	self deny: pipe atEnd.
	self assert: (pipe next == $c).
	self deny: pipe atEnd.
	pipe closeWriter.
	self assert: pipe atEnd.
	pipe close
