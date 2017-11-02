"
ZnHttpUnsuccessful is signalled when an HTTP client receives a non success response.

See ZnResponse>>#isSuccess

I hold the response that was unsuccessful.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnHttpUnsuccessful,
	#superclass : #Error,
	#instVars : [
		'response'
	],
	#category : 'Zinc-HTTP-Exceptions'
}

{ #category : #'instance creation' }
ZnHttpUnsuccessful class >> response: response [
	^ self new
		response: response;
		yourself
]

{ #category : #accessing }
ZnHttpUnsuccessful >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
ZnHttpUnsuccessful >> response [
	^ response
]

{ #category : #accessing }
ZnHttpUnsuccessful >> response: anObject [
	response := anObject
]

{ #category : #accessing }
ZnHttpUnsuccessful >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		response ifNotNil: [
			response statusLine printCodeAndReasonOn: stream ] ]
]
