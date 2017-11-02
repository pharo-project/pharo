"
ZnResponseWrittenEvent signals that response was written in duration milliseconds.
"
Class {
	#name : #ZnResponseWrittenEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'response',
		'duration'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnResponseWrittenEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnResponseWrittenEvent >> duration: anObject [
	duration := anObject
]

{ #category : #printing }
ZnResponseWrittenEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Response Written '; print: response; space; print: duration; << 'ms'
]

{ #category : #accessing }
ZnResponseWrittenEvent >> response [
	^ response
]

{ #category : #accessing }
ZnResponseWrittenEvent >> response: anObject [
	response := anObject
]
