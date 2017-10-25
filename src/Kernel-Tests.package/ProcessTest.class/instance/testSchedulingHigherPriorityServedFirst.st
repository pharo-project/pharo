tests
testSchedulingHigherPriorityServedFirst
    "The first process to run will pass straight through the gate
    while the other waits for the assert to whichRan."

	"Even though the low priority process is waiting longer, 
	the higher priority process should run first."

	| gate checkAssert whichRan |
	gate := Semaphore new signal. 
	checkAssert := Semaphore new.
	
	[ gate wait. whichRan := 11. checkAssert signal ] forkAt: 11.
	[ gate wait. whichRan := 12. checkAssert signal ] forkAt: 12.
	
	checkAssert wait.
	self assert: whichRan=12 description: 'Second scheduled but higher priority should run first'.	
	gate signal. 
	
	checkAssert wait.
	self assert: whichRan=11 description: 'First scheduled but lower priority should run after'.
