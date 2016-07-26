helpers
fork: aBlock

	| newProcess |
	newProcess := aBlock forkNamed: testSelector.
	forkedProcesses add: newProcess.
	^newProcess