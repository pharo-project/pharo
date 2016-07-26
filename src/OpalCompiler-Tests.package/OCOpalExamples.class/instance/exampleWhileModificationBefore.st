examples-blocks-optimized
exampleWhileModificationBefore
	| index |
	collection := OrderedCollection new.
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	 ^collection collect: #value.
	"#(5 5 5 5 5)"