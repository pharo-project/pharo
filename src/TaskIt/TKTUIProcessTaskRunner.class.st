Class {
	#name : #TKTUIProcessTaskRunner,
	#superclass : #TKTRunner,
	#category : #'TaskIt-Kernel'
}

{ #category : #scheduling }
TKTUIProcessTaskRunner >> isUIRunner [
	^ true
]

{ #category : #scheduling }
TKTUIProcessTaskRunner >> scheduleTaskExecution: aTaskExecution [
	self currentWorld defer: [ self executeTask: aTaskExecution ]
]
