"
I'm a generic command to perform actions where remote is the main actor.
Tipically, this is usually Pull and Push operations. My children will take care about it.
"
Class {
	#name : #IceTipRemoteActionCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'badge'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipRemoteActionCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational
		and: [ aToolContext repositoryModel hasRemotes ]
]

{ #category : #accessing }
IceTipRemoteActionCommand class >> isAbstract [ 
	^ self == IceTipRemoteActionCommand
]

{ #category : #execution }
IceTipRemoteActionCommand >> badge [
	^ badge
]

{ #category : #execution }
IceTipRemoteActionCommand >> badge: aNumber [
	"if zero reset to nil to make it dissapear"
	badge := aNumber = 0 
		ifTrue: [ nil ]
		ifFalse: [ aNumber ]
]

{ #category : #validating }
IceTipRemoteActionCommand >> validateRemotesPresent [
	self repositoryModel hasRemotes
		ifFalse: [ IceNoRemote signal  ]
]
