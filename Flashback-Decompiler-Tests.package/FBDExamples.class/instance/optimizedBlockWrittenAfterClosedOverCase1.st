examples
optimizedBlockWrittenAfterClosedOverCase1
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		[ temp ].
		temp := index := index + 1].
	^index
