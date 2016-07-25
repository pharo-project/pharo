render thread
enqueueOperation: operation blocking: blocking
	^ self enqueueOperation: operation blocking: blocking ifError: [ ]