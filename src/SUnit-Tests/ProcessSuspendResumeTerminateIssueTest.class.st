Class {
	#name : #ProcessSuspendResumeTerminateIssueTest,
	#superclass : #TestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #running }
ProcessSuspendResumeTerminateIssueTest >> runCaseManaged [
	"Here we are testing the test environment logic.
	So we should disable it for ourselves"
	
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #tests }
ProcessSuspendResumeTerminateIssueTest >> testMostSimpleProcess [
	| process ended |
	ended := false.
	process := [ ended := true ] newProcess.
	process priority: Processor activePriority + 1.
	process resume.
	self assert: ended description: 'process should be ended'.
	self assert: process isTerminated description: 'process should be terminated; current pc=', process suspendedContext pc asString
]

{ #category : #tests }
ProcessSuspendResumeTerminateIssueTest >> testWithSuspendInTheMiddleOfProcess [
	"The test discovers the issue with Process>>isTerminated method
	when process was suspended in the middle and then resumed.
	During the bootstrap process on CI it fails RunKernelTests stage.
	But it is working on full image"
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
	self assert: process isTerminated description: 'process should be terminated'
]

{ #category : #tests }
ProcessSuspendResumeTerminateIssueTest >> testWithoutSuspendInTheMiddleOfProcess [
	"The test proves that suspend at the middle is required condition to trigger the issue.
	Here the suspend is not performed and test passes on every CI stage.
	(notice that #printString call here is to have equivalent message send like #suspend)"	
	| process ended started |
	ended := false.
	started := false.
	process := [ started := true. self printString. ended := true ] newProcess.
	process priority: Processor activePriority + 1.
	process resume.
	self assert: started description: 'process should be started'.
	self assert: ended description: 'process should be ended'.
	self assert: process isTerminated description: 'process should be terminated'
]
