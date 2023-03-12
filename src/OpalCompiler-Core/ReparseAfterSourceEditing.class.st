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

{ #category : #handling }
ReparseAfterSourceEditing >> defaultAction [
	"Senders do not expect the exception to be resumed"

	self unhandledErrorAction
]

{ #category : #testing }
ReparseAfterSourceEditing >> isResumable [

	^false
]

{ #category : #accessing }
ReparseAfterSourceEditing >> newSource [

	^ newSource
]

{ #category : #accessing }
ReparseAfterSourceEditing >> newSource: anObject [

	newSource := anObject
]
