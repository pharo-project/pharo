process control
debugProcess: aProcess
	self resumeProcess: aProcess.
	aProcess debugWithTitle: 'Interrupted from the Process Browser'.
