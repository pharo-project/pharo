"
ZnRequestWrittenEvent signals that a request was written in duration milliseconds.
"
Class {
	#name : #ZnRequestWrittenEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'request',
		'duration'
	],
	#category : #Zinc-HTTP-Logging
}

{ #category : #accessing }
ZnRequestWrittenEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnRequestWrittenEvent >> duration: anObject [
	duration := anObject
]

{ #category : #printing }
ZnRequestWrittenEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Request Written '; print: request; space; print: duration; << 'ms'
]

{ #category : #accessing }
ZnRequestWrittenEvent >> request [
	^ request
]

{ #category : #accessing }
ZnRequestWrittenEvent >> request: anObject [
	request := anObject
]
