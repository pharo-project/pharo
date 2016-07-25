testing-while
testWhileModificationBefore
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)