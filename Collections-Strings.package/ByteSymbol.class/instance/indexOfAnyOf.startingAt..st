accessing
indexOfAnyOf: aCollection startingAt: start
	"Use double dispatching for speed"
	^aCollection findFirstInByteString: self startingAt: start