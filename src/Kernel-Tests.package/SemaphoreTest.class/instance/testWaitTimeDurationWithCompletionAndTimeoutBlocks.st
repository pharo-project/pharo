tests
testWaitTimeDurationWithCompletionAndTimeoutBlocks
	"Ensure that wait:onCompletion:onTimeout: behaves properly"

	"Ensure that a timed out wait:onCompletion:onTimeout: returns the value of the timeout block"
	self assert: (Semaphore new 
		wait: 50 milliSeconds
		onCompletion: [ #completed ]
		onTimeout: [ #timeout ] ) == #timeout.

	"Ensure that a signaled wait:onCompletion:onTimeout: returns the value of the completed block"
	self assert: (Semaphore new signal
		wait: 50 milliSeconds
		onCompletion: [ #completed ]
		onTimeout: [ #timeout ]) == #completed.
