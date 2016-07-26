instance creation
signalLock: aSemaphore afterMSecs: anInteger inMonitor: aMonitor queue: anOrderedCollection
	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ (self new setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection) schedule; yourself