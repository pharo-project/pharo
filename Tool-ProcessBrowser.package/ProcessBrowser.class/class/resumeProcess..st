process control
resumeProcess: aProcess
	| priority |
	priority := self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess suspendedContext ifNotNil: [ aProcess resume ]
