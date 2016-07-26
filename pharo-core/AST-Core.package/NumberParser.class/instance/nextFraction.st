parsing-public
nextFraction
	| numerator denominator |
	numerator := self nextInteger.
	(sourceStream peekFor: $/) ifFalse: [^numerator].
	denominator := self nextInteger.
	^numerator / denominator