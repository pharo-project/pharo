tests
testSemaphoreTimeout
	"When we provide our own semaphore for a Delay, it should be used"
	"See http://bugs.squeak.org/view.php?id=6834"

	"self run: #testSemaphoreTimeout"
	
	| sem |
	sem := Semaphore new.
	[ | process |
		process := [Delay timeoutSemaphore: sem afterMSecs: 0. sem wait] newProcess.
		process priority: Processor highIOPriority.
		process resume.
		self assert: process isTerminated.
	] ensure: [sem signal]