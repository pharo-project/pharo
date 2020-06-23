"
I model temp write.
"
Class {
	#name : #RFTempWrite,
	#superclass : #RFVariableWrite,
	#instVars : [
		'context'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFTempWrite >> context [
	^ context
]

{ #category : #accessing }
RFTempWrite >> context: anObject [
	context := anObject
]

{ #category : #evaluating }
RFTempWrite >> value [
	variable write: assignedValue inContext: context.
	^assignedValue
]

{ #category : #accessing }
RFTempWrite >> variableName: aSymbol [
	variable := context temporaryVariableNamed: aSymbol 
]
