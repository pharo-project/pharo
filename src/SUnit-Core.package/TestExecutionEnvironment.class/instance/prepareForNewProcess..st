controlling
prepareForNewProcess: aProcess
	| processBlock |
	watchDogProcess ifNil: [ ^self ].  "we should not catch watchDogProcess"
	forkedProcesses add: aProcess.
	aProcess suspendedContext sender ifNotNil: [ ^self ]. "Some existing tests in system create processes on arbitrary block and then check suspendedContext state. Without this 'if' all this tests will fail"
	processBlock := aProcess suspendedContext receiver.
	processBlock isClosure ifFalse: [ ^self ]. "same case as in previous comment".
	
	aProcess on: Error do: [ :err | | activeProcess |
		"It is possible to fork process with copy of stack from another process
		It means that aProcess could be not actual active process inside this block handler.
		So we should not reference temp aProcess and instead we should work with actual active process"
		activeProcess := Processor activeProcess. 
		failedProcesses at: activeProcess put: err.
		activeProcess isTerminating ifFalse: [activeProcess suspend].
		err pass]