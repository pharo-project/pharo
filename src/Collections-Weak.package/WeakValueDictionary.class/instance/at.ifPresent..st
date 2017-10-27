adding
at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^ (array at: (self findElementOrNil: key)) value
			ifNotNil: [ :assoc | aBlock cull: assoc value enclosedSetElement ]