tests - on-fork
testOnForkErrorOnSeparateProcess
	"Test that if code runs with error, there is fork"
	
	| result forkedProc sema |
	
	sema := Semaphore new.	
	result := [ 1/0 ] on: Exception fork: [ forkedProc := Processor activeProcess. sema signal ].

	sema wait.
	self assert: (forkedProc ~~ Processor activeProcess).