examples
optimizedBlockWrittenAfterClosedOverCase2
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		 [ temp ] ].
