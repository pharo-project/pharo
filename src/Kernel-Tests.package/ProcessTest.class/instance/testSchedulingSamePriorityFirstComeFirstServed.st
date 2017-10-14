tests
testSchedulingSamePriorityFirstComeFirstServed
    "The first process to run will pass straight through the gate
    while the other waits for the assert to whichRan."

	"At the same priority process, the longer waiting process should run first."

	| gate checkAssert whichRan |
	gate := Semaphore new signal. 
	checkAssert := Semaphore new.
		
	[ gate wait. whichRan := 1. checkAssert signal ] fork.
	[ gate wait. whichRan := 2. checkAssert signal ] fork.
	
	checkAssert wait.
	self assert: whichRan=1 description: 'First scheduled process should run first'.
	gate signal. 

	checkAssert wait.
	self assert: whichRan=2 description: 'Second scheduled process should run after'.