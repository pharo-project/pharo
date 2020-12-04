"
I represent the state of a scheduled task. A task can be either be ready to be run,  cancelled, running or finished.
"
Class {
	#name : #TKTTaskState,
	#superclass : #Object,
	#category : #'TaskIt-Kernel'
}

{ #category : #cancelling }
TKTTaskState >> cancelTaskExecution: aTaskExecution reason: aReason [

	self subclassResponsibility
]

{ #category : #testing }
TKTTaskState >> isFinished [

	^ false
]

{ #category : #testing }
TKTTaskState >> isRunning [

	^ false
]

{ #category : #executing }
TKTTaskState >> performTaskExecution: aTaskExecution [

	self subclassResponsibility
]
