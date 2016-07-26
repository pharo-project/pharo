stream-protocol
at: first write: aCollection startingAt: start count: count 
	| last |
	last := first + count - 1.
	last > bytes size ifTrue: [ self grownBy: last - size ].
	bytes 
		replaceFrom: first
		to: last
		with: aCollection
		startingAt: start.
	size := last