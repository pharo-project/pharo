inheriting
installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess
	super installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess.
	
	anExecutionEnvironment prepareForNewProcess: newProcess