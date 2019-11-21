"
The standard TestExecutionEnvironment kills all the processes forked during the test execution when the test stops without errors or with a time-out exception. That makes debugging of tests that use side-processes very hard because if such a process causes an error.

This execution environment does not clean the process so you need to take responsibility for them. 

-------------------------------------------------------------------
  Use this testing environment only during debugging of the tests
-------------------------------------------------------------------
"
Class {
	#name : #TestExecutionEnvironmentWithoutCleaning,
	#superclass : #TestExecutionEnvironment,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #controlling }
TestExecutionEnvironmentWithoutCleaning >> checkForkedProcesses [
]

{ #category : #controlling }
TestExecutionEnvironmentWithoutCleaning >> prepareForNewProcess: aProcess [

	forkedProcesses add: aProcess.

]
