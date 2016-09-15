private
failedTestWithFailedChildProcessTest

	[self error: 'error from child process'] forkNamed: #failedChildProcessTest.
	Processor yield.
	self error: 'failed test with failed child process'