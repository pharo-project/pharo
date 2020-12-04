"
I am a generic task that can be created from a valuable
"
Class {
	#name : #TKTGenericTask,
	#superclass : #TKTTask,
	#instVars : [
		'valuable'
	],
	#category : #'TaskIt-Kernel'
}

{ #category : #executing }
TKTGenericTask >> evaluateOn: aTaskExecution [
	^ valuable value
]

{ #category : #accessing }
TKTGenericTask >> valuable [
	^ valuable
]

{ #category : #accessing }
TKTGenericTask >> valuable: aValuable [
	valuable := aValuable
]
