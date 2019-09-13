"
I am the implementation of the decorator design pattern for command group.

I just delegate everything to the #decoratedGroup. My subclasses probably do something more interesting.
"
Class {
	#name : #CmCommandGroupDecorator,
	#superclass : #CmAbstractCommandsGroup,
	#traits : 'CmTDecorator',
	#classTraits : 'CmTDecorator classTrait',
	#instVars : [
		'decoratedGroup'
	],
	#category : #'Commander2-Groups'
}

{ #category : #accessing }
CmCommandGroupDecorator >> allCommands [
	^ self decoratedGroup allCommands
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> commandOrGroupNamed: aString [
	^ self decoratedGroup commandOrGroupNamed: aString
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> commands [
	^ self decoratedGroup commands
]

{ #category : #decorating }
CmCommandGroupDecorator >> decorate: aCmCommandsGroup [
	self decoratedGroup: aCmCommandsGroup
]

{ #category : #accessing }
CmCommandGroupDecorator >> decoratedGroup [
	^ decoratedGroup
]

{ #category : #accessing }
CmCommandGroupDecorator >> decoratedGroup: anObject [
	decoratedGroup := anObject
]

{ #category : #accessing }
CmCommandGroupDecorator >> description [
	^ self decoratedGroup description
]

{ #category : #accessing }
CmCommandGroupDecorator >> description: aString [
	self decoratedGroup description: aString
]

{ #category : #accessing }
CmCommandGroupDecorator >> dynamicDescription [
	^ self decoratedGroup dynamicDescription
]

{ #category : #accessing }
CmCommandGroupDecorator >> dynamicName [
	^ self decoratedGroup dynamicName
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> entries [
	^ self decoratedGroup entries
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> groups [
	^ self decoratedGroup groups
]

{ #category : #testing }
CmCommandGroupDecorator >> hasCommand: aCmCommand [
	^ self decoratedGroup hasCommand: aCmCommand
]

{ #category : #testing }
CmCommandGroupDecorator >> hasEntryNamed: aString [
	^ self decoratedGroup hasEntryNamed: aString
]

{ #category : #accessing }
CmCommandGroupDecorator >> name [
	^ self decoratedGroup name
]

{ #category : #accessing }
CmCommandGroupDecorator >> name: aString [
	self decoratedGroup name: aString
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> register: aCmCommandOrGroup [
	self decoratedGroup register: aCmCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroupDecorator >> unregister: aCmCommandOrGroup [
	^ self decoratedGroup unregister: aCmCommandOrGroup
]
