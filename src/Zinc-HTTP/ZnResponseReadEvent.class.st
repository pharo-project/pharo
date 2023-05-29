"
ZnResponseReadEvent signals that a response was read in duration milliseconds. This time includes all waiting, server processing and networking.
"
Class {
	#name : #ZnResponseReadEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'response',
		'duration'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnResponseReadEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnResponseReadEvent >> duration: anObject [
	duration := anObject
]

{ #category : #printing }
ZnResponseReadEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Response Read '; print: response; space; print: duration; << 'ms'
]

{ #category : #accessing }
ZnResponseReadEvent >> response [
	^ response
]

{ #category : #accessing }
ZnResponseReadEvent >> response: anObject [
	response := anObject
]
