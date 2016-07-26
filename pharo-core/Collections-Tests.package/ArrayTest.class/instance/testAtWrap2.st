tests - accessing
testAtWrap2
	| tabTest |
	tabTest := #(5 6 8 ).
	self assert: (tabTest atWrap: 2) = 6.
	self assert: (tabTest atWrap: 7) = 5.
	self assert: (tabTest atWrap: 5) = 6.
	self assert: (tabTest atWrap: 0) = 8.
	self assert: (tabTest atWrap: 1) = 5.
	self assert: (tabTest atWrap: -2) = 5