helpers
fork: aBlock at: priority

	| newProcess |
	newProcess := aBlock forkAt: priority named: testSelector.
	forkedProcesses add: newProcess.
	^newProcess