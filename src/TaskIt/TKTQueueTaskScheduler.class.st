"
I am an abstract task scheduler that schedules tasks inside a queue to be consumed afterwards
"
Class {
	#name : #TKTQueueTaskScheduler,
	#superclass : #Object,
	#traits : 'TTaskScheduler',
	#classTraits : 'TTaskScheduler classTrait',
	#instVars : [
		'queue'
	],
	#category : #'TaskIt-Worker'
}

{ #category : #initialization }
TKTQueueTaskScheduler >> initialize [

	super initialize.
	queue := AtomicSharedQueue new
]

{ #category : #testing }
TKTQueueTaskScheduler >> isEmpty [

	^ queue isEmpty
]

{ #category : #schedulling }
TKTQueueTaskScheduler >> purge [

	queue removeAll
]

{ #category : #accessing }
TKTQueueTaskScheduler >> queue: aTaskQueue [

	queue := aTaskQueue
]

{ #category : #schedulling }
TKTQueueTaskScheduler >> scheduleTaskExecution: aTask [
	
	queue nextPut: aTask
]
