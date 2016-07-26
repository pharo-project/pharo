examples
singleRemoteTempVar
	| index block theCollection |
	index := 0.
	block := [ 
		theCollection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)