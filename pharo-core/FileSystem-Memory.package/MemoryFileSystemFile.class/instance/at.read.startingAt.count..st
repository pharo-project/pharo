stream-protocol
at: index read: aCollection startingAt: start count: count 
	| max stop |
	max := size - index + 1 min: count.
	stop := start + max - 1.
	aCollection 
		replaceFrom: start
		to: stop
		with: bytes
		startingAt: index.
	^ stop - start + 1