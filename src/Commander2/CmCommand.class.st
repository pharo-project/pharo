"
I am the common superclass to all commands defined in the context of Commander2.

My subclasses need to redefine at least #execute method.
This method defines the behaviour of the command.

Eventually, #canBeRun can be override to check if the command can be run according to its current context.
"
Class {
	#name : #CmCommand,
	#superclass : #CmAbstractCommand,
	#traits : 'CmTWithBasicNameAndDescription + CmTDecorable',
	#classTraits : 'CmTWithBasicNameAndDescription classTrait + CmTDecorable classTrait',
	#instVars : [
		'context'
	],
	#category : #'Commander2-Commands'
}

{ #category : #default }
CmCommand class >> defaultDescription [
	^ 'Not described command.'
]

{ #category : #default }
CmCommand class >> defaultName [ 

	^ 'Unnamed'
]

{ #category : #testing }
CmCommand >> canBeExecuted [
	"See my super class comment for this method.
	 By default I return true.
	"
	^ true
]

{ #category : #accessing }
CmCommand >> context [
	"The #context might be a BlockClosure that computes the actual context object when needed.
	 Thus, we send #value which in case of a BlockClosure evaluate it and in case of another
	 Object, simply returns itself.
	"
	^ context value
]

{ #category : #accessing }
CmCommand >> context: anObject [
	context := anObject
]

{ #category : #testing }
CmCommand >> hasContext [
	^ self context isNotNil
]

{ #category : #initialization }
CmCommand >> initialize [
	super initialize.
	self name: self class defaultName.
	self description: self class defaultDescription
]
