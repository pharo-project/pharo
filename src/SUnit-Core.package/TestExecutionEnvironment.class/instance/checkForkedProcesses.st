controlling
checkForkedProcesses

	forkedProcesses reject: [:each | failedProcesses includesKey: each ] thenDo: #terminate.
	failedProcesses ifEmpty: [ ^self].	
	"Following curtailed logic ensures that we will terminate all processes only 
	if we will not debug them"
	[TestFailedByForkedProcess signal] ifCurtailed: [failedProcesses keysDo: #terminate]. 
	"We will be here only if user press Proceed in debugger. 
	In that case we will allow him to debug failed processes"
	failedProcesses keys reject: #isTerminated thenDo: #resume