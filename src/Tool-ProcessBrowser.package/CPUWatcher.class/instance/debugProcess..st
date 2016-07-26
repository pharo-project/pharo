process operations
debugProcess: aProcess
	| uiPriority  |
	uiPriority := Processor activeProcess priority.
	aProcess priority >= uiPriority ifTrue: [
		self processBrowser setProcess: aProcess toPriority: uiPriority - 1
	].
	self processBrowser debugProcess: aProcess.