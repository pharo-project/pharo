printing
printOn: aStream

	self isExited
		ifTrue: [aStream nextPutAll: 'normal termination with status ';
				 nextPutAll: self statusIfExited asString].
	self isSignaled
		ifTrue: [aStream nextPutAll: 'exit due to signal ';
				nextPutAll: self statusIfSignaled asString].
	self isStopped
		ifTrue: [aStream nextPutAll: 'stopped due to signal ';
				nextPutAll: self statusIfStopped].