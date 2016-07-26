tests
testBlockTemporariesBoxed
	| text temps context |
	text := 'testIt
	| a b c |
	a := [ :each | |d | d = a ifTrue:[ |x|  ] ifFalse:[ |y|.
	^self'.
	context := self 
		createContextFor: text
		at: 73.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 5.
	self assert: temps first = 'a'.
	self assert: temps second = 'b'.
	self assert: temps third = 'c'.
	self assert: temps fourth = 'each'.
	self assert: temps fifth = 'd'.
