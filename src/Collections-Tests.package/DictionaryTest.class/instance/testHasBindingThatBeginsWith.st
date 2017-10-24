tests - testing
testHasBindingThatBeginsWith
	| newDict |
	newDict := self collectionClass new 
		at: #abc put: 10; 
		at: #abcd put: 100; 
		at: #def put: 20; 
		yourself.
	self assert: (newDict hasBindingThatBeginsWith: 'ab').
	self assert: (newDict hasBindingThatBeginsWith: 'def').
	self deny: (newDict hasBindingThatBeginsWith: 'defg').