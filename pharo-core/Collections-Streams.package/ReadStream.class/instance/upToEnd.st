accessing
upToEnd
	| start |
	start := 1 + position.
	position := readLimit.
	^collection copyFrom: start to: position