"
Abstract superclass for all variable accesss. 
"
Class {
	#name : #RFVariableAccess,
	#superclass : #Object,
	#instVars : [
		'variable'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #evaluating }
RFVariableAccess >> value [
	self subclassResponsibility
]

{ #category : #accessing }
RFVariableAccess >> variable [
	^variable
]

{ #category : #accessing }
RFVariableAccess >> variable: anObject [
	variable := anObject
]

{ #category : #accessing }
RFVariableAccess >> variableName: aSymbol [
	self subclassResponsibility
]
