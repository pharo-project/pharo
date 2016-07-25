private
evaluateSelectionNumber: n
	| i start stop |
	i := start := 1.
	[stop := morph text indexOf: $% startingAt: start + 1 ifAbsent: morph text size + 1.
	i = n]
		whileFalse:
			[i := i + 1.
			start := stop + 1].
	morph editor selectFrom: start to: stop - 1.
	^self evaluateSelection
	