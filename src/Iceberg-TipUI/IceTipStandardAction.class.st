"
I'm a generic UI action.
I will perform an action by informing user what's happening.
I will also check for errors happening while performing the task and try to recover from them.
"
Class {
	#name : #IceTipStandardAction,
	#superclass : #IceTipAction,
	#instVars : [
		'message',
		'successAnnounceBlock',
		'actionBlock'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipStandardAction >> action [
	^ actionBlock
]

{ #category : #accessing }
IceTipStandardAction >> action: aBlock [
	actionBlock := aBlock
]

{ #category : #executing }
IceTipStandardAction >> basicExecute [
	UIManager default 
		informUser: self message
		during: actionBlock.
	successAnnounceBlock 
		ifNotNil: [ Iceberg announcer announce: successAnnounceBlock value ]
]

{ #category : #executing }
IceTipStandardAction >> do: aBlock [
	self action: aBlock.
	self execute
]

{ #category : #executing }
IceTipStandardAction >> execute: aBlock [
	self action: aBlock.
	self execute
]

{ #category : #accessing }
IceTipStandardAction >> message [
	^ message
]

{ #category : #accessing }
IceTipStandardAction >> message: anObject [
	message := anObject
]

{ #category : #accessing }
IceTipStandardAction >> onSuccessAnnounce: aValuable [
	successAnnounceBlock := aValuable
]

{ #category : #accessing }
IceTipStandardAction >> onSuccessRepositoryModified [
	^ self onSuccessAnnounce: [ IceRepositoryModified for: self repository ]
]
