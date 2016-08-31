tests
testWaitTimeDuration
	"Ensure that wait: aDuration behaves properly"

	"Ensure that a timed out wait: aDuration returns true from the wait"
	self assert: (Semaphore new wait: 50 milliSeconds) == true.

	"Ensure that a signaled wait: aDuration returns false from the wait"
	self assert: (Semaphore new signal wait: 50 milliSeconds) == false.
