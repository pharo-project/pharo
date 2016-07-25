tests
testWaitTimeoutMSecs
	"Ensure that waitTimeoutMSecs behaves properly"

	"Ensure that a timed out waitTimeoutMSecs: returns true from the wait"
	self assert: (Semaphore new waitTimeoutMSecs: 50) == true.

	"Ensure that a signaled waitTimeoutMSecs: returns false from the wait"
	self assert: (Semaphore new signal waitTimeoutMSecs: 50) == false.
