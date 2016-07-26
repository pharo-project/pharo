render thread
renderThreadProcess
	| session |
	session := Smalltalk session.
	
	[session == Smalltalk session] whileTrue: [
		"Execute the enqueued rendering operations"
		OperationAvailableSemaphore consumeAllSignals.
		self executeRenderOperations.
		
		"Animations must be refreshed periodically"
		self performAnimationFrames
	]


