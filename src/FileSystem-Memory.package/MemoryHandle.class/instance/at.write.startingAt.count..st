public
at: first write: aCollection startingAt: start count: count 
	writable ifFalse: [ self primitiveFailed ].
	entry at: first write: aCollection startingAt: start count: count.