"
I represent a ready to run task. A ready task will start running when asked, or be cancelled immediately if cancelled.
"
Class {
	#name : #TKTReadyTaskState,
	#superclass : #TKTTaskState,
	#category : #'TaskIt-Kernel'
}

{ #category : #cancelling }
TKTReadyTaskState >> cancelTaskExecution: aTaskExecution reason: aReason [

	aTaskExecution setCancelStateWithReason: aReason
]

{ #category : #cancelling }
TKTReadyTaskState >> performTaskExecution: aTaskExecution [
	aTaskExecution setRunningState.
	aTaskExecution doExecuteTask.
	aTaskExecution setFinishedState
]
