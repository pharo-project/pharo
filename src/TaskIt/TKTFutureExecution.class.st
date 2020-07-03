"
I represent a task execution with a result: I have an associated future where I will deploy my result, as either a success or a failure.
"
Class {
	#name : #TKTFutureExecution,
	#superclass : #TKTTaskExecution,
	#instVars : [
		'future'
	],
	#category : #'TaskIt-Futures'
}

{ #category : #evaluating }
TKTFutureExecution >> doExecuteTask [
	[ future deploySuccess: super doExecuteTask ]
		on: Error
		do: [ :error | future deployFailure: error freeze ]
]

{ #category : #accessing }
TKTFutureExecution >> future [

	^ future
]

{ #category : #initialization }
TKTFutureExecution >> initialize [

	super initialize.
	future := TKTFuture new.
	
]

{ #category : #accessing }
TKTFutureExecution >> runner: aRunner [

	super runner: aRunner.
	future runner: aRunner.
]
