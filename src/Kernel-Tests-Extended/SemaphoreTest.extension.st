Extension { #name : #SemaphoreTest }

{ #category : #'*Kernel-Tests-Extended' }
SemaphoreTest >> testWaitAndWaitTimeoutTogether [
	| semaphore value waitProcess waitTimeoutProcess |
	semaphore := Semaphore new.

	waitProcess := [ semaphore wait.
	value := #wait ] fork.

	waitTimeoutProcess := [ semaphore waitTimeoutMSecs: 50.
	value := #waitTimeout ] fork.

	"Wait for the timeout to happen"
	(Delay forMilliseconds: 100) wait.

	"The waitTimeoutProcess should already have timed out.  This should release the waitProcess"
	semaphore signal.

	[ waitProcess isTerminated and: [ waitTimeoutProcess isTerminated ] ] whileFalse: [ (Delay forMilliseconds: 100) wait ].

	self assert: value equals: #wait
]
