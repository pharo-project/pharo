helpers
timeoutAfter: aDuration ifNot: aBlock

	| timer |
	timer := Stopwatch new activate.
	[ timer duration < aDuration and: [ aBlock value not ] ] whileTrue.