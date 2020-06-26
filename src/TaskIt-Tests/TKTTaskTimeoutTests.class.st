Class {
	#name : #TKTTaskTimeoutTests,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTTaskTimeoutTests >> testTaskDoesNotTimeoutIfTaskExecutionIsShorter [

	| runner finished |
	
	finished := false.
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	runner schedule: [ 10 milliSeconds wait. finished := true ] asTask timeout: 50 milliSeconds.
	
	self assert: runner exceptionHandler error equals: nil.
	self assert: finished
]

{ #category : #tests }
TKTTaskTimeoutTests >> testTaskTimeoutIfTaskExecutionIsLonger [
	| runner |
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: TKTTestExceptionHandler new.
	runner schedule: [ 50 milliSeconds wait ] asTask timeout: 10 milliSeconds.
	self assert: (runner exceptionHandler error isKindOf: TKTCancelledTaskException)
]
