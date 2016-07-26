testing
testAddAndRemoveMethodsFromSubtraits
	| aC2 |
	aC2 := self c2 new.
	self assert: aC2 m51.
	self t5 removeSelector: #m51.
	self should: [ aC2 m51 ] raise: MessageNotUnderstood.
	self t1 compile: 'foo ^true'.
	self deny: aC2 foo.
	self t1 compile: 'm51 ^self'.
	aC2 m51.
	self assert: aC2 m51 == aC2