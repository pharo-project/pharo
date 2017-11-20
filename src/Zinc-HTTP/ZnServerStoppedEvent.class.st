"
ZnServerStoppedEvent notes the shutdown of an HTTP server is complete.
"
Class {
	#name : #ZnServerStoppedEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'description'
	],
	#category : #Zinc-HTTP-Logging
}

{ #category : #accessing }
ZnServerStoppedEvent >> description [
	^ description
]

{ #category : #accessing }
ZnServerStoppedEvent >> description: anObject [
	description := anObject
]

{ #category : #printing }
ZnServerStoppedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Stopped '; << description
]
