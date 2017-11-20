"
ZnClientRetryingEvent signals a client is retrying is main operation. The number of retries is configurable.

"
Class {
	#name : #ZnClientRetryingEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'exception'
	],
	#category : #Zinc-HTTP-Logging
}

{ #category : #accessing }
ZnClientRetryingEvent >> exception [
	^ exception
]

{ #category : #accessing }
ZnClientRetryingEvent >> exception: anObject [
	exception := anObject
]

{ #category : #printing }
ZnClientRetryingEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Retrying '; print: exception
]
