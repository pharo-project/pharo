controlling
prepareForNewProcess: aProcess
	| processBlock args |
	watchDogProcess ifNil: [ ^self ].  "we should not catch watchDogProcess"
	forkedProcesses add: aProcess.
	aProcess suspendedContext sender ifNotNil: [ ^self ]. "it is not clean how to inject error handler into process which created on arbitrary context"
	processBlock := aProcess suspendedContext receiver.
	processBlock isClosure ifFalse: [ ^self ]. "same case as in previous comment".
	
	args := aProcess suspendedContext arguments.
	args ifNotEmpty: [ args := args first  "it the case when process created from block with args" ].

	aProcess install: [ 
			[processBlock valueWithArguments: args.
			Processor terminateActive] on: Error do: [ :err | 
				failedProcesses at: aProcess put: err.
				aProcess suspend.
				err pass]] asContext