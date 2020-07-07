"
I represent a cancelled task state. A cancelled task will fail to execute with an exception
"
Class {
	#name : #TKTCancelledTaskState,
	#superclass : #TKTTaskState,
	#instVars : [
		'reason'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #'instance creation' }
TKTCancelledTaskState class >> reason: aString [ 
	
	^ self new
		reason: aString;
		yourself
]

{ #category : #cancelling }
TKTCancelledTaskState >> cancelTaskExecution: aTaskExecution reason: aReason [

	self error: 'Task already cancelled'
]

{ #category : #executing }
TKTCancelledTaskState >> performTaskExecution: aTaskExecution [

	self signalCancelledException
]

{ #category : #accessing }
TKTCancelledTaskState >> reason: aString [ 
	reason := aString
]

{ #category : #executing }
TKTCancelledTaskState >> signalCancelledException [

	TKTCancelledTaskException signal: reason
]
