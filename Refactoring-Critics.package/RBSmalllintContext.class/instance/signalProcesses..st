private
signalProcesses: aSemaphore 
	aSemaphore isNil ifTrue: [^self].
	[aSemaphore isEmpty] whileFalse: [aSemaphore signal]