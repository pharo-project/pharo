private
signalQueue: anOrderedCollection
	queuesMutex critical: [
	 anOrderedCollection isEmpty ifFalse: [
			anOrderedCollection removeFirst signal ] ]