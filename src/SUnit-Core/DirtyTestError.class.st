"
I am a root of hierarchy of errors which represent the dirty state of system produced by test.
For example the test can left the running background process or background process can raise an error.

TestExecutionEnvironment checks the system state at the end of test and concrete error is raised if dirty state is detected. 
Those errors are signalled even if test was failed due to domain error. In that case when domain error is handled by SUnit (marking the test result as failed) the another error is signaled to notify about bad system state
"
Class {
	#name : #DirtyTestError,
	#superclass : #Error,
	#instVars : [
		'executionEnvironment'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'instance creation' }
DirtyTestError class >> signalFrom: aTestExecutionEnvironment [
	^self new
		executionEnvironment: aTestExecutionEnvironment;
		signal
]

{ #category : #accessing }
DirtyTestError >> executionEnvironment [

	^ executionEnvironment
]

{ #category : #accessing }
DirtyTestError >> executionEnvironment: anObject [

	executionEnvironment := anObject
]

{ #category : #'exception handling' }
DirtyTestError >> manageTestProcessBy: aProcessMonitorTestService [
	"Do nothing because I am not an error during actual test execution.
	My subclasses only represent the dirty system state at the end of test"
]

{ #category : #'exception handling' }
DirtyTestError >> sunitAnnounce: aTestCase toResult: aTestResult [

	executionEnvironment isMainTestProcessFailed ifTrue: [
		"The main test process errors are handled by SUnit using same message.
		So the actual domain test error is already counted in result"
		^ self ].

	"If main test process was completed successfully
	we should mark test as failed due to dirty system state"
	aTestResult addError: aTestCase
]
