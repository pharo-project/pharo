private
hangedTestDueToFailedChildProcess
	self timeLimit: 10 milliSeconds.
	
	self failedChildProcessTest.
	
	20 milliSeconds wait.

	
	