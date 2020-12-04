Class {
	#name : #TKTNewProcessTaskRunnerTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTNewProcessTaskRunnerTest >> testNewProcessTaskRunnerExceptionIsHandledByExceptionHandler [
	| runner error |
	
	runner := TKTNewProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	error := Error new.
	runner schedule: [ error signal ].
	
	"We wait until the error is signaled, trapped and we have it."
	100 milliSeconds wait.
	self assert: runner exceptionHandler error equals: error
]

{ #category : #tests }
TKTNewProcessTaskRunnerTest >> testNewProcessTaskRunnerRunsInNewProcessEveryTime [

	| runner hashes futures hashesSet |
	runner := TKTNewProcessTaskRunner new.
	hashes := AtomicSharedQueue new.
	
	futures := (1 to: 10) collect: [ :i | (runner future: [ Processor activeProcess identityHash ]) onSuccessDo: [ :v | hashes nextPut: v ] ].
	
	futures do: [ :each | each waitForCompletion: 2 seconds ].
	
	hashesSet := Set new.
	[ hashes isEmpty ] whileFalse: [ hashesSet add: hashes next ].
	self assert: hashesSet size equals: 10.
]

{ #category : #tests }
TKTNewProcessTaskRunnerTest >> testNewProcessTaskRunnerRunsNeverInLocalProcess [

	| runner hashes futures |
	runner := TKTNewProcessTaskRunner new.
	hashes := AtomicSharedQueue new.
	
	futures := (1 to: 10) collect: [ :i | (runner future: [ Processor activeProcess identityHash ]) onSuccessDo: [ :v | hashes nextPut: v ] ].
	
	futures do: [ :each | each waitForCompletion: 2 seconds ].
	
	[ hashes isEmpty ] whileFalse: [ 
		self assert: hashes next ~= Processor activeProcess identityHash ].
]
