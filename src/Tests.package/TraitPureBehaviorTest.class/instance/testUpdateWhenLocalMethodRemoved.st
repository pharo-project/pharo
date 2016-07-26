testing-applying trait composition
testUpdateWhenLocalMethodRemoved
	| aC2 |
	aC2 := self c2 new.
	self t5 compile: 'foo ^123'.
	self deny: aC2 foo.
	self c2 removeSelector: #foo.
	self assert: aC2 foo = 123