process actions
resumeProcess
	selectedProcess
		ifNil: [^ self].
	self class resumeProcess: selectedProcess.
	self updateProcessList