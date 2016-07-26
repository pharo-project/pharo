tests
testReturnsAsManyWaitsAsSignalsReceived

	| semaphore semaphoreReturnedOnce semaphoreReturnedTwice |
	semaphoreReturnedOnce := semaphoreReturnedTwice := false.
	semaphore := Semaphore new.
	
	[
		semaphore wait.
		semaphoreReturnedOnce := true.
		semaphore wait.
		semaphoreReturnedTwice := true ] fork.

	Processor yield.
	self deny: semaphoreReturnedOnce.
	
	semaphore signal.
	Processor yield.
	self assert: semaphoreReturnedOnce.
	self deny: semaphoreReturnedTwice.
	
	semaphore signal.
	Processor yield.
	self assert: semaphoreReturnedTwice.