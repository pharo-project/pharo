helpers
assertTerminatedFailedChildProcessesFor: aTestCase

	| failedProcesses |
	failedProcesses := aTestCase failedChildProcesses.
	self assert: failedProcesses notEmpty.
	self assert: (failedProcesses allSatisfy: #isTerminated) 