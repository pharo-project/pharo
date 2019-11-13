"
I am the implementation of the decorator design pattern for commands.

I just delegate everything to the #decoratedCommand. My subclasses probably do something more interesting.
"
Class {
	#name : #CmCommandDecorator,
	#superclass : #CmAbstractCommand,
	#traits : 'CmTDecorator',
	#classTraits : 'CmTDecorator classTrait',
	#instVars : [
		'decoratedCommand'
	],
	#category : #'Commander2-Commands'
}

{ #category : #testing }
CmCommandDecorator >> canBeExecuted [
	"This hook allows the command decide if it can be run with the context it holds or not.
	 It should return a boolean.
	"
	^ self decoratedCommand canBeExecuted
]

{ #category : #accessing }
CmCommandDecorator >> context [
	^ self decoratedCommand context
]

{ #category : #accessing }
CmCommandDecorator >> context: anObject [
	self decoratedCommand context: anObject
]

{ #category : #decorating }
CmCommandDecorator >> decorate: aCmCommand [
	self decoratedCommand: aCmCommand
]

{ #category : #accessing }
CmCommandDecorator >> decoratedCommand [
	^ decoratedCommand
]

{ #category : #accessing }
CmCommandDecorator >> decoratedCommand: anObject [
	decoratedCommand := anObject
]

{ #category : #accessing }
CmCommandDecorator >> description [
	^ self decoratedCommand description
]

{ #category : #accessing }
CmCommandDecorator >> description: aString [
	self decoratedCommand description: aString
]

{ #category : #accessing }
CmCommandDecorator >> dynamicDescription [
	^ self decoratedCommand dynamicDescription
]

{ #category : #accessing }
CmCommandDecorator >> dynamicName [
	^ self decoratedCommand dynamicName
]

{ #category : #executing }
CmCommandDecorator >> execute [
	self decoratedCommand execute
]

{ #category : #accessing }
CmCommandDecorator >> name [
	^ self decoratedCommand name
]

{ #category : #accessing }
CmCommandDecorator >> name: aString [
	self decoratedCommand name: aString
]
