handling
logTranscript
	self showWarning 
		ifTrue: [Transcript nextPutAll: self messageText; cr; flush].