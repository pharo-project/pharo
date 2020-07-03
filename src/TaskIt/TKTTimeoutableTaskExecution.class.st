"
I am a task execution that is automatically cancelled with a timeout.
"
Class {
	#name : #TKTTimeoutableTaskExecution,
	#superclass : #TKTTaskExecution,
	#instVars : [
		'timeout'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #evaluating }
TKTTimeoutableTaskExecution >> doExecuteTask [
	TKTConfiguration runner scheduleTimeout: timeout on: self.
	^ super doExecuteTask
]

{ #category : #accessing }
TKTTimeoutableTaskExecution >> timeout: aTimeout [

	timeout := aTimeout
]
