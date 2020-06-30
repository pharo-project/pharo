Class {
	#name : #ProcessTerminationBugTest,
	#superclass : #TestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #running }
ProcessTerminationBugTest >> runCaseManaged [
	"Here we are testing the test environment logic.
	So we should disable it for ourselves"
	
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #tests }
ProcessTerminationBugTest >> testHighPriorityProcessDoesNotFinishCorrectly [
	| process ended |
	ended := false.
	process := [ ended := true ] newProcess.
	process priority: Processor activePriority + 1.
	process resume.
	self assert: ended description: 'process should be ended'.
	self 
		assert: process isTerminated 
		description: 'process should be terminated; current pc='
			, process suspendedContext pc asString, ' in ', process suspendedContext printString
]
