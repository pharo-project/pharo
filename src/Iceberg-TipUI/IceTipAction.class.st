"
I'm like a lightweight command, used in general for apply concrete actions. 
"
Class {
	#name : #IceTipAction,
	#superclass : #Object,
	#instVars : [
		'repository',
		'successBlock'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipAction class >> iconNamed: aSymbol [
	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : #executing }
IceTipAction >> basicExecute [
	self subclassResponsibility

]

{ #category : #executing }
IceTipAction >> execute [
	self withErrorHandlingDo: [ 
		self basicExecute.
		self finishSuccess ]
]

{ #category : #private }
IceTipAction >> finishSuccess [
	successBlock ifNotNil: [ successBlock value ]
]

{ #category : #accessing }
IceTipAction >> onSuccess: aBlock [ 
	"Some actions need to execute something right after finish. Like a refresh of panels, etc.
	 If a block is defined here, it will be evaluated right after the command is executed, if 
	 there are no exceptions."
	successBlock := aBlock
]

{ #category : #accessing }
IceTipAction >> repository [
	^ repository
]

{ #category : #accessing }
IceTipAction >> repository: anObject [
	repository := anObject
]

{ #category : #private }
IceTipAction >> withErrorHandlingDo: aBlock [
	aBlock
	on: IceError, IceWarning
	do: [ :e | e acceptError: (IceTipInteractiveErrorVisitor on: self repository) ].

]
