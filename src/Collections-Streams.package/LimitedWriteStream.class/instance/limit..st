accessing
limit: anInteger
	limit := anInteger.

	position > limit
		ifTrue: [
			position := limit.
			limitBlock value ]