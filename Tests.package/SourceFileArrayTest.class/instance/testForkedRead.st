testing
testForkedRead

	| originalString remoteString readSemaphore readString testSemaphore |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.

	self ensureChangesFileOpenedInProcess.
	
	[	self ensureChangesFileOpenedInProcess.

		readSemaphore wait.

		"Read the string that was written in other process."
		readString := remoteString string.
		testSemaphore signal 
		] fork.

	"Write the string, that will be read in other process."
	remoteString := SourceFiles remoteStringForNewString: originalString.

	readSemaphore signal.
	testSemaphore wait.
	
	self assert: readString equals: originalString.