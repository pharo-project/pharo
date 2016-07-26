testing
testBlockingPeek

		| pipe |
	pipe := self blockingPipe.
	pipe nextPutAll: 'abc'.
	pipe flush. "protect against buggy clib that may never flush output"
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
