"
I am used to request a new author name.
"
Class {
	#name : 'AuthorNameRequest',
	#superclass : 'Notification',
	#instVars : [
		'initialAnswer'
	],
	#category : 'System-Support-Authoring',
	#package : 'System-Support',
	#tag : 'Authoring'
}

{ #category : 'accessing' }
AuthorNameRequest class >> isDeprecated [
	^ true
]

{ #category : 'accessing' }
AuthorNameRequest class >> messagePrompt [
	^
'Please type your full name.
It will be used to sign the changes you make to the image.
Spaces, accents, dashes, underscore and similar characters are not allowed
'
]

{ #category : 'exception handling' }
AuthorNameRequest >> defaultAction [
	^ nil
]

{ #category : 'accessing' }
AuthorNameRequest >> initialAnswer [

	^ initialAnswer
]

{ #category : 'accessing' }
AuthorNameRequest >> initialAnswer: anObject [

	initialAnswer := anObject
]

{ #category : 'ui-requests' }
AuthorNameRequest >> messagePrompt [
	^ self class messagePrompt
]
