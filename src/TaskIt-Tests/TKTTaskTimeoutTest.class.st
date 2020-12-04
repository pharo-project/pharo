Class {
	#name : #TKTTaskTimeoutTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTTaskTimeoutTest >> testTaskDoesNotTimeoutIfTaskExecutionIsShorter [

	| runner finished |
	
	finished := false.
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	runner schedule: [ 10 milliSeconds wait. finished := true ] asTask timeout: 50 milliSeconds.
	
	self assert: runner exceptionHandler error equals: nil.
	self assert: finished
]

{ #category : #tests }
TKTTaskTimeoutTest >> testTaskTimeoutIfTaskExecutionIsLonger [
	| runner |
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: TKTTestExceptionHandler new.
	runner schedule: [ 50 milliSeconds wait ] asTask timeout: 10 milliSeconds.
	self assert: (runner exceptionHandler error isKindOf: TKTCancelledTaskException)
]
