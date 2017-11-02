"
ZnRequestResponseHandledEvent signals that the handler (delegate) processed a request and produced a response in duration in milliseconds. This duration is thus the time taken by the application itself.
"
Class {
	#name : #ZnRequestResponseHandledEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'request',
		'response',
		'duration'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnRequestResponseHandledEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnRequestResponseHandledEvent >> duration: anObject [
	duration := anObject
]

{ #category : #printing }
ZnRequestResponseHandledEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Request Handled '; print: request; space; print: duration; << 'ms'
]

{ #category : #accessing }
ZnRequestResponseHandledEvent >> request [
	^ request
]

{ #category : #accessing }
ZnRequestResponseHandledEvent >> request: anObject [
	request := anObject
]

{ #category : #accessing }
ZnRequestResponseHandledEvent >> response [
	^ response
]

{ #category : #accessing }
ZnRequestResponseHandledEvent >> response: anObject [
	response := anObject
]
