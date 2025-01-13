"
ZnClientIgnoringExceptionOnConnectionReuseEvent signals that a client connection was reused and that an exception occurred. Normally, there is one retry after that, trying to reconnect.

"
Class {
	#name : #ZnClientIgnoringExceptionOnConnectionReuseEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'exception'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnClientIgnoringExceptionOnConnectionReuseEvent >> exception [
	^ exception
]

{ #category : #accessing }
ZnClientIgnoringExceptionOnConnectionReuseEvent >> exception: anObject [
	exception := anObject
]

{ #category : #printing }
ZnClientIgnoringExceptionOnConnectionReuseEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Ignoring Exception On Connection Reuse '; print: exception
]
