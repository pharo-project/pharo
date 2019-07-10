"
I am a message send. I am here as a subsclass so we have all operations in one spot
"
Class {
	#name : #RFReturnOperation,
	#superclass : #Object,
	#instVars : [
		'context',
		'returnValue'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFReturnOperation >> context [
	^ context
]

{ #category : #accessing }
RFReturnOperation >> context: anObject [
	context := anObject
]

{ #category : #accessing }
RFReturnOperation >> returnValue [
	^ returnValue
]

{ #category : #accessing }
RFReturnOperation >> returnValue: anObject [
	returnValue := anObject
]

{ #category : #evaluating }
RFReturnOperation >> value [
	context return: returnValue 
]
