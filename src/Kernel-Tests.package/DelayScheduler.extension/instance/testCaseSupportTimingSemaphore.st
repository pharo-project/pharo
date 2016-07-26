*Kernel-Tests
testCaseSupportTimingSemaphore
	"Tests might alter which sempahore the VM signals, so call this from test tearDown"
	^ timingSemaphore.