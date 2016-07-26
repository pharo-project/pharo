testing-while
testWhileModificationBeforeNotInlined
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)