tests - on-fork
testOnForkErrorReturnsNil
	"Test that if code runs with error, there is fork"
	
	| result sema |
	
	sema := Semaphore new.	
	result := [ 1/0 ] on: Exception fork: [ sema signal. ].
	
	sema wait.
	"in case of error, evaluation result should be nil"
	self assert: result isNil.