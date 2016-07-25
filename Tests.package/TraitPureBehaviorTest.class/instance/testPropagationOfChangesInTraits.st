testing-applying trait composition
testPropagationOfChangesInTraits
	| aC2 |
	aC2 := self c2 new.
	self assert: self c2 methodDict size = 9.
	self t1 compile: 'zork ^false'.
	self assert: self c2 methodDict size = 10.
	self deny: aC2 zork.
	self t1 removeSelector: #m12.
	self assert: self c2 methodDict size = 9.
	self should: [aC2 m12] raise: MessageNotUnderstood.
	self assert: aC2 m21 = 21.
	self t2 compile: 'm21 ^99'.
	self assert: aC2 m21 = 99