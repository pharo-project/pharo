Class {
	#name : #ProcessSpecialIsTerminatTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Extended-Processes'
}

{ #category : #running }
ProcessSpecialIsTerminatTest >> runCaseManaged [
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #tests }
ProcessSpecialIsTerminatTest >> testSuspendResumeTerminateBug [
	| process ended started |
	ended := false.
	started := false.
	process := [ started := true. Processor activeProcess suspend. ended := true ] newProcess.
	process priority: Processor activePriority + 1.
	process resume.
	self assert: started description: 'process should be started'.
	self assert: process isSuspended description: 'process should be suspended'.

	process resume.
	self assert: ended description: 'process should be ended'.
	self assert: process isTerminated -> process suspendedContext pc equals: true -> 190 "it's to log the issue"
]
