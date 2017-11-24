"
I am ZnServerGenericLogEvent, a ZnServerLogEvent and ZnLogEvent that holds a generic subject, possibly not limited to a String.

Here are two examples:

(ZnServerGenericLogEvent subject: 'You can''t do that, Dave') emit.

(ZnServerGenericLogEvent subject: { #id->123. #reason->#timeout } asDictionary) emit.
"
Class {
	#name : #ZnServerGenericLogEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'subject'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #'instance creation' }
ZnServerGenericLogEvent class >> subject: object [
	"self subject: #(i 10 j -1)."
	"self subject: 'How are you?'."
	
	^ self new
		subject: object;
		yourself
]

{ #category : #printing }
ZnServerGenericLogEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream space; nextPutAll: subject asString
]

{ #category : #accessing }
ZnServerGenericLogEvent >> subject [
	^ subject
]

{ #category : #accessing }
ZnServerGenericLogEvent >> subject: anObject [
	subject := anObject
]
