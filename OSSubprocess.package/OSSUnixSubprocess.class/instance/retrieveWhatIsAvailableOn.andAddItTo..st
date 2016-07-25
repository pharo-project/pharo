streams
retrieveWhatIsAvailableOn: aStream andAddItTo: accumulatingStream
	"This is an internal method which should only be used IF the user will NOT
	retrieve contents himself from streams while the process is still running.
	The usecase of this method is #runAndWaitOnExitDo: and #waitForExitPollingEvery:retrievingStreams:
	in which we are sure the user will not be reading from the streams. 
	What this method does is to simply get what is available in aStream and write it in  accumulatingStream, which is 
	the one we are using for appending contents (so that it is available all togther when the process has exited)
	This is important because for example, for pipes, you cannot read something twice as you consume what you read. 
	
	 "
	aStream 
		ifNotNil: [ :str | 
			accumulatingStream nextPutAll: str upToEnd 
		].