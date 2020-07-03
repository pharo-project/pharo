"
I represent a running task. A running task cannot be asked to be executed while it is running. Cancelling a running task will throw an exception and interrupt its execution.
"
Class {
	#name : #TKTRunningTaskState,
	#superclass : #TKTTaskState,
	#category : #'TaskIt-Kernel'
}

{ #category : #cancelling }
TKTRunningTaskState >> cancelTaskExecution: aTaskExecution reason: aReason [

	| cancelState activeContext timeoutContext |

	cancelState := TKTCancelledTaskState reason: aReason.
	
	activeContext := aTaskExecution executionProcess suspendedContext.
	timeoutContext := [ cancelState signalCancelledException ] asContext.
	timeoutContext privSender: activeContext.
	aTaskExecution executionProcess suspendedContext: timeoutContext.
	aTaskExecution setCancelStateWithReason: aReason
]

{ #category : #testing }
TKTRunningTaskState >> isRunning [

	^ true
]

{ #category : #executing }
TKTRunningTaskState >> performTaskExecution: aTaskExecution [

	self error: 'task is already running'
]
