process control
setProcess: aProcess toPriority: priority
	| oldPriority |
	oldPriority := self suspendedProcesses at: aProcess ifAbsent: [ ].
	oldPriority ifNotNil: [ self suspendedProcesses at: aProcess put: priority ].
	aProcess priority: priority.
	^oldPriority