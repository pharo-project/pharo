tests
testLists
	self assert: (STONJSON toString: #(1 2 3)) equals: '[1,2,3]'.
	self assert: (STONJSON toString: #(1 -2 true 3 nil)) equals: '[1,-2,true,3,null]'.
	self assert: (STONJSON toString: #(1 (2) 3)) equals: '[1,[2],3]'.
	self assert: (STONJSON toString: #()) equals: '[]'.
	self assert: (STONJSON fromString: '[]') equals: #().
	self assert: (STONJSON fromString: '[1,2,3]') equals: #(1 2 3).
	self assert: (STONJSON fromString: '[1,-2,true,3,null]') equals: #(1 -2 true 3 nil).
	self assert: (STONJSON fromString: '[1,[2],3]') equals: #(1 (2) 3).