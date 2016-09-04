initialization
setLimit: anInteger limitBlock: aBlock
	"Limit the numer of elements this stream will write..."
	limit := anInteger.

	"Execute this (typically ^ contents) when that limit is exceded"
	limitBlock := aBlock.

	"To support legacy users, this test must be here too"
	position > limit
		ifTrue: [
			position := limit.
			limitBlock value ]