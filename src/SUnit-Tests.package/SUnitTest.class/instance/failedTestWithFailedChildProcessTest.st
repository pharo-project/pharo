private
failedTestWithFailedChildProcessTest

	self failedChildProcessTest.
	Processor yield.
	self error: 'failed test with failed child process'