"
I'm an error (notification) signaled when a part of a class definition is not recognized. 
"
Class {
	#name : #CDUnrecognizedClassPart,
	#superclass : #Notification,
	#instVars : [
		'unrecognizedPart',
		'classDefinition'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDUnrecognizedClassPart >> classDefinition: aNode [
	classDefinition := aNode
]

{ #category : #accessing }
CDUnrecognizedClassPart >> unrecognizedPart: aString [ 
	unrecognizedPart := aString
]
