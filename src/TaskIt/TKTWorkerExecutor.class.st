"
I am an object that will be used as executor of the TKTWorker. 
In that way I don't need to copy the TKTWorker and nilling its instance variables.
"
Class {
	#name : #TKTWorkerExecutor,
	#superclass : #Object,
	#instVars : [
		'processToKill'
	],
	#category : #'TaskIt-Worker'
}

{ #category : #finalization }
TKTWorkerExecutor >> finalize [

	processToKill kill
]

{ #category : #accessing }
TKTWorkerExecutor >> processToKill [
	^ processToKill
]

{ #category : #accessing }
TKTWorkerExecutor >> processToKill: anObject [
	processToKill := anObject
]
