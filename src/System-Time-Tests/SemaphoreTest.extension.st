Extension { #name : 'SemaphoreTest' }

{ #category : '*System-Time-Tests' }
SemaphoreTest >> testWaitTimeDuration [
	"Ensure that wait: aDuration behaves properly"

	"Ensure that a timed out wait: aDuration returns true from the wait"

	self assert: (Semaphore new wait: 50 milliSeconds) identicalTo: true.

	"Ensure that a signaled wait: aDuration returns false from the wait"
	self assert: (Semaphore new signal wait: 50 milliSeconds) identicalTo: false
]

{ #category : '*System-Time-Tests' }
SemaphoreTest >> testWaitTimeDurationWithCompletionAndTimeoutBlocks [
	"Ensure that wait:onCompletion:onTimeout: behaves properly"

	"Ensure that a timed out wait:onCompletion:onTimeout: returns the value of the timeout block"

	self assert: (Semaphore new wait: 50 milliSeconds onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #timeout.

	"Ensure that a signaled wait:onCompletion:onTimeout: returns the value of the completed block"
	self assert: (Semaphore new signal wait: 50 milliSeconds onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #completed
]
