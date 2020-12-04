Class {
	#name : #TKTLocalProcessTaskRunnerTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTLocalProcessTaskRunnerTest >> testLocalProcessTaskRunnerExceptionIsHandledByExceptionHandler [

	| runner error |
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	error := Error new.
	runner schedule: [ error signal ].
	
	self assert: runner exceptionHandler error equals: error
]

{ #category : #tests }
TKTLocalProcessTaskRunnerTest >> testLocalProcessTaskRunnerRunsInLocalProcess [

	| runner future got |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ Processor activeProcess identityHash ].
	
	future onSuccessDo: [ :value | got := value ].
	future waitForCompletion: 2 seconds.
	
	self assert: got equals: Processor activeProcess identityHash.
]
