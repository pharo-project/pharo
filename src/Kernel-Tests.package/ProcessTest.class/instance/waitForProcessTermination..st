helpers
waitForProcessTermination: aProcess
	aProcess priority: Processor activePriority + 1.
	[ aProcess isTerminated ] whileFalse: [ Processor yield ]