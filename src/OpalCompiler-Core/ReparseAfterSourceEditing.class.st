"
Notify about the need to reparse after source code was edited
"
Class {
	#name : #ReparseAfterSourceEditing,
	#superclass : #Notification,
	#instVars : [
		'newSource'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #signaling }
ReparseAfterSourceEditing class >> signalWithNewSource: aSourceCode [

	^ self new
		newSource: aSourceCode;
		signal
]

{ #category : #accessing }
ReparseAfterSourceEditing >> newSource [
	
	^ newSource
]

{ #category : #accessing }
ReparseAfterSourceEditing >> newSource: anObject [
	
	newSource := anObject
]
