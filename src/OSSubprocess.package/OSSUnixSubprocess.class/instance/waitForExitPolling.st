waiting
waitForExitPolling
	"Read the comment of waitForExitPollingEvery:retrievingStreams:"
	^ self waitForExitPollingEvery: (Delay forMilliseconds: 50)