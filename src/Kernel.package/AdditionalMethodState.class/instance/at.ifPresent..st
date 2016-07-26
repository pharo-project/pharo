accessing
at: aKey ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer self."
	^ aBlock value: (self at: aKey ifAbsent: [ ^ self ])