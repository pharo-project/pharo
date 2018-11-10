"
I'm a generic iceberg command. 
My children implement real functionality available through iceberg tools.
"
Class {
	#name : #IceTipCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'repositoryModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipCommand class >> defaultHelp [
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipCommand class >> defaultPosition [
	^ #left
]

{ #category : #accessing }
IceTipCommand class >> defaultToolbarItemName [
	^ self defaultMenuItemName
]

{ #category : #accessing }
IceTipCommand class >> isAbstract [
	^ self == IceTipCommand
]

{ #category : #accessing }
IceTipCommand >> badge [
	^ nil
]

{ #category : #accessing }
IceTipCommand >> defaultHelp [
	^ self class defaultHelp
]

{ #category : #accessing }
IceTipCommand >> defaultPosition [
	^ self class defaultPosition
]

{ #category : #accessing }
IceTipCommand >> defaultToolbarItemName [
	^ self class defaultToolbarItemName
]

{ #category : #execution }
IceTipCommand >> readParametersFromContext: aToolContext [
	repositoryModel := aToolContext repositoryModel
]

{ #category : #accessing }
IceTipCommand >> repositoryModel [
	^ repositoryModel
]

{ #category : #enumerating }
IceTipCommand >> withErrorHandlingDo: aBlock [
	aBlock
	on: IceError 
	do: [ :e | e acceptError: (IceTipInteractiveErrorVisitor on: self repositoryModel entity) ]
]
