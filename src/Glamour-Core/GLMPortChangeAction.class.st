Class {
	#name : #GLMPortChangeAction,
	#superclass : #GLMAction,
	#instVars : [
		'portName'
	],
	#category : #'Glamour-Core'
}

{ #category : #accessing }
GLMPortChangeAction >> portName [
	^ portName
]

{ #category : #accessing }
GLMPortChangeAction >> portName: anObject [
	portName := anObject
]
