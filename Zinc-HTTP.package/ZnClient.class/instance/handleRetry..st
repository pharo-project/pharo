private protocol
handleRetry: exception
	self logRetrying: exception.
	self close.
	(Delay forSeconds: self retryDelay) wait.
