tests - on-fork
testOnForkErrorExecutesBlock
	"Test that if code runs with error, there is fork"
	
	| result sema |
	
	sema := Semaphore new.	
	result := nil.
	[ 1/0 ] on: Exception fork: [ result := 2. sema signal].
		
	sema wait.	
	"and of course result should be not nil "
	self assert: result = 2.