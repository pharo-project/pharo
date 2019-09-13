"
While other commands actions should be defined by overriding #execute method, I'm a block command. 
I  execute a block as my action.

"
Class {
	#name : #CmBlockCommand,
	#superclass : #CmCommand,
	#instVars : [
		'block',
		'canBeExecutedBlock'
	],
	#category : #'Commander2-Commands'
}

{ #category : #accessing }
CmBlockCommand >> block [
	^ block
]

{ #category : #accessing }
CmBlockCommand >> block: anObject [
	block := anObject
]

{ #category : #testing }
CmBlockCommand >> canBeExecuted [
	self hasContext
		ifFalse: [ ^ super canBeExecuted ].	"When no context is set, we consider the command to be runnable. So we call super."
	^ self canBeExecutedBlock value: self context
]

{ #category : #accessing }
CmBlockCommand >> canBeExecutedBlock [
	^ canBeExecutedBlock
]

{ #category : #accessing }
CmBlockCommand >> canBeExecutedBlock: anObject [
	canBeExecutedBlock := anObject
]

{ #category : #executing }
CmBlockCommand >> execute [
	self hasContext
		ifTrue: [ block value: self context ]
		ifFalse: [ block value ]
]

{ #category : #initialization }
CmBlockCommand >> initialize [
	super initialize.
	self canBeExecutedBlock: [ :ctxt | true ]
]
