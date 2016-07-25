tests
testTemporaries
	| text temps context |
	text := 'testIt: aRectangle
	| a b c |
	a _ [ :each | |d | ^d].
	^self'.
	context := self 
		createContextFor: text
		at: text size.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 4.
	self assert: temps first = 'aRectangle'.
	self assert: temps second = 'a'.
	self assert: temps third = 'b'.
	self assert: temps fourth = 'c'