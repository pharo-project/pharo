tests
testBlockTemporaries
	| text temps context |
	text := 'testIt
	| a b c |
	a _ [ :each | |d | ^d].
	^self'.
	context := self 
		createContextFor: text
		at: 39.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 5.
	self assert: temps first = 'a'.
	self assert: temps second = 'b'.
	self assert: temps third = 'c'.
	self assert: temps fourth = 'each'.
	self assert: temps fifth = 'd'