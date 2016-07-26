as yet unclassified
setLimit: sizeLimit limitBlock: aBlock
	"Limit the numer of elements this stream will write..."
	limit := sizeLimit.
	"Execute this (typically ^ contents) when that limit is exceded"
	limitBlock := aBlock