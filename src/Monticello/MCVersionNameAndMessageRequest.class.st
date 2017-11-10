"
Notify to requests for version name and message
"
Class {
	#name : #MCVersionNameAndMessageRequest,
	#superclass : #Notification,
	#instVars : [
		'suggestion',
		'suggestedLogComment'
	],
	#category : #Monticello-Versioning
}

{ #category : #accessing }
MCVersionNameAndMessageRequest >> suggestedLogComment [

	^ suggestedLogComment
]

{ #category : #accessing }
MCVersionNameAndMessageRequest >> suggestedLogComment: aLogMessage [

	suggestedLogComment := aLogMessage
]

{ #category : #accessing }
MCVersionNameAndMessageRequest >> suggestedName [
	^ suggestion
]

{ #category : #accessing }
MCVersionNameAndMessageRequest >> suggestedName: aString [
	suggestion := aString
]
