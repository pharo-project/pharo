logging
logFailure: aString
	self isLogging ifTrue:
		[self failureLog cr; nextPutAll: aString; flush].