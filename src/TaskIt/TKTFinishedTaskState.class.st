"
I represent an already finished task state.  Both cancelling and performing a task that is already finished results in an exception.
"
Class {
	#name : #TKTFinishedTaskState,
	#superclass : #TKTTaskState,
	#category : #'TaskIt-Kernel'
}

{ #category : #cancelling }
TKTFinishedTaskState >> cancelTaskExecution: aTaskExecution reason: aReason [

	self error: 'Task already finished'
]

{ #category : #testing }
TKTFinishedTaskState >> isFinished [

	^ true
]

{ #category : #executing }
TKTFinishedTaskState >> performTaskExecution: aTaskExecution [

	self error: 'Task already finished'
]
