tests
testWaitTimeoutSecondsOnCompletionOnTimeout
	"Ensure that waitTimeoutSeconds:onCompletion:onTimeout: behaves properly"

	"Ensure that a timed out waitTimeoutSeconds:onCompletion:onTimeout: returns the value of the timeout block"
	self assert: (Semaphore new 
		waitTimeoutSeconds: 0.05
		onCompletion: [ #completed ]
		onTimeout: [ #timeout ] ) == #timeout.

	"Ensure that a signaled waitTimeoutSeconds:onCompletion:onTimeout: returns the value of the completed block"
	self assert: (Semaphore new signal
		waitTimeoutSeconds: 0.05
		onCompletion: [ #completed ]
		onTimeout: [ #timeout ]) == #completed.
