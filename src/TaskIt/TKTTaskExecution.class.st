"
I represent the execution of a given task. I know if the task is finished or not and I can cancel it while it is being executed.
"
Class {
	#name : #TKTTaskExecution,
	#superclass : #Object,
	#instVars : [
		'runner',
		'task',
		'executionProcess',
		'isRunning',
		'cancelled',
		'state'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #evaluating }
TKTTaskExecution >> cancel [

	self cancelWithReason: ''
]

{ #category : #evaluating }
TKTTaskExecution >> cancelWithReason: reason [

	state cancelTaskExecution: self reason: reason
]

{ #category : #evaluating }
TKTTaskExecution >> doExecuteTask [
	^ task evaluateOn: self
]

{ #category : #evaluating }
TKTTaskExecution >> executeTask [ 
	^ state performTaskExecution: self 
]

{ #category : #accessing }
TKTTaskExecution >> executionProcess [

	^ executionProcess
]

{ #category : #initialization }
TKTTaskExecution >> initialize [
	super initialize.
	state := TKTReadyTaskState new.

]

{ #category : #testing }
TKTTaskExecution >> isFinished [

	^ state isFinished
]

{ #category : #testing }
TKTTaskExecution >> isRunning [

	^ state isRunning
]

{ #category : #testing }
TKTTaskExecution >> noteRunningOn: aRunner [ 
	" Some times is needed to know the real runner. By default we do nothing :) "
]

{ #category : #accessing }
TKTTaskExecution >> runner: aRunner [

	runner := aRunner
]

{ #category : #states }
TKTTaskExecution >> setCancelStateWithReason: aReason [

	state := TKTCancelledTaskState new
]

{ #category : #states }
TKTTaskExecution >> setFinishedState [

	state := TKTFinishedTaskState new
]

{ #category : #states }
TKTTaskExecution >> setReadyState [
	state := TKTReadyTaskState new
]

{ #category : #states }
TKTTaskExecution >> setRunningState [

	state := TKTRunningTaskState new
]

{ #category : #evaluating }
TKTTaskExecution >> task: aTask [

	task := aTask
]

{ #category : #evaluating }
TKTTaskExecution >> value [
	executionProcess := Processor activeProcess.
	TKTConfiguration runner: runner during: [ self executeTask ].
]
