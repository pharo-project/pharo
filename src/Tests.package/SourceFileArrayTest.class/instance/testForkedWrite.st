testing
testForkedWrite

	| originalString remoteString readSemaphore readString testSemaphore |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.
	
	self ensureChangesFileOpenedInProcess.

	[	self ensureChangesFileOpenedInProcess.
		
		readSemaphore wait.

		"Write the string, that will be read in other process."
		remoteString := SourceFiles remoteStringForNewString: originalString.

		testSemaphore signal 
		] fork.

	readSemaphore signal.
	testSemaphore wait.

	"Read the string that was written in other process."
	readString := remoteString string.
	
	self assert: readString equals: originalString.