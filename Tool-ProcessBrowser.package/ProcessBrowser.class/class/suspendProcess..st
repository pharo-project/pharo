process control
suspendProcess: aProcess
	| priority |
	priority := aProcess priority.
	self suspendedProcesses at: aProcess put: priority.
	"Need to take the priority down below the caller's
	so that it can keep control after signaling the Semaphore"
	(aProcess suspendingList isKindOf: Semaphore)
		ifTrue: [aProcess priority: Processor lowestPriority.
			aProcess suspendingList signal].
	[aProcess suspend]
		on: Error
		do: [:ex | self suspendedProcesses removeKey: aProcess].
	aProcess priority: priority.
