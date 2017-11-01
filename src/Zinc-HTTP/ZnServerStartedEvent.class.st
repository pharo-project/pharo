"
ZnServerStartedEvent notes the startup of an HTTP server is complete.

"
Class {
	#name : #ZnServerStartedEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'description'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnServerStartedEvent >> description [
	^ description
]

{ #category : #accessing }
ZnServerStartedEvent >> description: anObject [
	description := anObject
]

{ #category : #printing }
ZnServerStartedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Started '; << description
]
