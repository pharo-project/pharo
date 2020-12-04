Class {
	#name : #TKTUIProcessTaskRunner,
	#superclass : #TKTRunner,
	#instVars : [
		'process'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #schedulling }
TKTUIProcessTaskRunner >> isUIRunner [
	^ true 
]

{ #category : #schedulling }
TKTUIProcessTaskRunner >> scheduleTaskExecution: aTaskExecution [
	WorldState addDeferredUIMessage: [ self executeTask: aTaskExecution ]
]

{ #category : #polimorfsms }
TKTUIProcessTaskRunner >> start [

]
