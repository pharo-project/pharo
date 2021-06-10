Class {
	#name : #TKTWorkerTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Worker'
}

{ #category : #tests }
TKTWorkerTest >> testWorkerExceptionIsHandledByExceptionHandler [

	| error worker |
	worker := TKTWorker new.
	worker exceptionHandler: (TKTTestExceptionHandler new).
	worker start.
	
	error := Error new.
	worker schedule: [ error signal ].
	
	"We wait until the error is signaled, trapped and we have it."
	100 milliSeconds wait.
	self assert: worker exceptionHandler error equals: error
]

{ #category : #tests }
TKTWorkerTest >> testWorkerProcessDiesAfterStop [

	| worker future |
	worker := TKTWorker new.
	worker start.
	worker stop.
	future := TKTFuture doing: [ [ worker isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	future synchronizeTimeout: 2 seconds.
	self deny: worker isRunning
]

{ #category : #tests }
TKTWorkerTest >> testWorkerProcessDiesAfterWorkerAndAllFuturesAreCollected [

	| worker future process waitFuture1 waitFuture2 |
	worker := TKTWorker new.
	worker start.
	process := worker process.
	waitFuture1 := TKTFuture doing: [ 
		               [ process isRunning ] whileTrue: [ 
			               50 milliSeconds wait ] ].
	waitFuture2 := TKTFuture doing: [ 
		               [ process isRunning ] whileTrue: [ 
			               50 milliSeconds wait ] ].
	future := worker future: [ 10 milliSeconds wait ].
	worker := nil.
	self waitGarbageCollect.
	self
		should: [ waitFuture1 synchronizeTimeout: 200 milliSeconds ]
		raise: TKTTimeoutException.
	self assert: process isRunning.
	future := nil.
	self waitGarbageCollect.
	self waitGarbageCollect.
	waitFuture2 synchronizeTimeout: 400 milliSeconds.
	self deny: process isRunning
]

{ #category : #tests }
TKTWorkerTest >> testWorkerProcessDiesAfterWorkerIsCollected [

	| worker future process |
	worker := TKTWorker new.
	worker start.
	process := worker process.
	worker := nil.
	self waitGarbageCollect.
	future := TKTFuture doing: [ 
		          [ process isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	future synchronizeTimeout: 2 seconds.
	self deny: process isRunning
]

{ #category : #tests }
TKTWorkerTest >> testWorkerProcessIsWorkingUntilAllTasksAreDone [

	| worker process waitFuture |
	worker := TKTWorker new.
	worker start.
	process := worker process.
	waitFuture := TKTFuture doing: [ 
		              [ process isRunning ] whileTrue: [ 
			              50 milliSeconds wait ] ].
	worker schedule: [ 100 milliSeconds wait ].
	self assert: process taskQueue size equals: 1.
	50 milliSeconds wait.
	self assert: process taskQueue size equals: 0.
	worker := nil.
	self assert: process taskQueue size equals: 0.
	self assert: process isRunning.
	self waitGarbageCollect.
	waitFuture waitForCompletion: 100 second.
	self deny: process isRunning
]

{ #category : #tests }
TKTWorkerTest >> testWorkerRunsAlwaysInSameProcess [

	| worker hash futures hashes |
	worker := TKTWorker new.
	worker start.
	
	futures := (1 to: 10) collect: [ :i | worker future: [ hash := Processor activeProcess identityHash ] ].
	hashes := futures collect: [ :each | each synchronizeTimeout: 1 hour ].
	
	self assert: hashes asSet size equals: 1
]

{ #category : #tests }
TKTWorkerTest >> testWorkerRunsInSeparateProcess [

	| worker hash future |
	worker := TKTWorker new.
	worker start.
	
	future := worker future: [ hash := Processor activeProcess identityHash ].
	future waitForCompletion: 1 hour.
	
	self assert: hash ~= Processor activeProcess identityHash.
]
