initialize - release
initializeSignalHandlers
	"#sigChldSemaphore will lazily create and register the semaphore if needed"
	self sigChldSemaphore.
	