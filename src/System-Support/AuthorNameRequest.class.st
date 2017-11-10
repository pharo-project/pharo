"
I am used to request a new author name.
"
Class {
	#name : #AuthorNameRequest,
	#superclass : #Notification,
	#instVars : [
		'initialAnswer'
	],
	#category : #System-Support
}

{ #category : #accessing }
AuthorNameRequest class >> messagePrompt [
	^
'Please type your full name. 
It will be used to sign the changes you make to the image.
Spaces, accents, dashes, underscore and similar characters are not allowed
'
]

{ #category : #'exception handling' }
AuthorNameRequest >> defaultAction [
	^ UIManager default
			request: self messagePrompt
			initialAnswer: initialAnswer
			title: 'Author identification'
]

{ #category : #accessing }
AuthorNameRequest >> initialAnswer [
	
	^ initialAnswer
]

{ #category : #accessing }
AuthorNameRequest >> initialAnswer: anObject [
	
	initialAnswer := anObject
]

{ #category : #ui-requests }
AuthorNameRequest >> messagePrompt [
	^ self class messagePrompt
]
