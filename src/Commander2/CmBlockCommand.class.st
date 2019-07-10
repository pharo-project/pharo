"
While other commands actions should be defined by overriding #execute method, I'm a block command. 
I  execute a block as my action.

"
Class {
	#name : #CmBlockCommand,
	#superclass : #CmCommand,
	#instVars : [
		'block',
		'canBeRunBlock'
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

{ #category : #hooks }
CmBlockCommand >> canBeRun [
	self hasContext "When no context is set, we consider the command to be runnable. So we call super."
		ifFalse: [ ^ super canBeRun ].
		
	^ self canBeRunBlock value: self context
]

{ #category : #accessing }
CmBlockCommand >> canBeRunBlock [
	^ canBeRunBlock
]

{ #category : #accessing }
CmBlockCommand >> canBeRunBlock: anObject [
	canBeRunBlock := anObject
]

{ #category : #execution }
CmBlockCommand >> execute [
	self hasContext
		ifTrue: [ block value: self context ]
		ifFalse: [ block value ]
]

{ #category : #initialization }
CmBlockCommand >> initialize [
	super initialize.
	self canBeRunBlock: [ :ctxt | true ]
]
