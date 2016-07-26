accessing
nextPut: anObject
	"Add anObject into the generator. A synonym to #yield: and value:."

	| previous |
	previous := next.
	next := anObject.
	continue := thisContext swapSender: continue.
	^ previous