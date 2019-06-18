"
I represent composition of existing context of given tool.
I always delegate command operations to context which is active for given operation. 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	existingContexts:		<Collection of<CmdToolContext>>
"
Class {
	#name : #CmdCompositeToolContext,
	#superclass : #CmdToolContext,
	#instVars : [
		'existingContexts'
	],
	#category : #'Commander-Core'
}

{ #category : #accessing }
CmdCompositeToolContext >> activeContextFor: aCommand [
	^existingContexts detect: [ :each | 
		each allowsExecutionOf: aCommand]
]

{ #category : #testing }
CmdCompositeToolContext >> allowsExecutionOf: aCommandClass [

	^existingContexts anySatisfy: [:each | 
		each allowsExecutionOf: aCommandClass]
]

{ #category : #'command execution' }
CmdCompositeToolContext >> applyResultOf: aCommand [

	(self activeContextFor: aCommand)
		applyResultOf: aCommand
]

{ #category : #accessing }
CmdCompositeToolContext >> existingContexts [
	^ existingContexts
]

{ #category : #accessing }
CmdCompositeToolContext >> existingContexts: anObject [
	existingContexts := anObject
]

{ #category : #'command execution' }
CmdCompositeToolContext >> prepareFullExecutionOf: aCommand [

	(self activeContextFor: aCommand)
		prepareFullExecutionOf: aCommand
]

{ #category : #'command execution' }
CmdCompositeToolContext >> prepareNewCommand: aCommand [
	
	(self activeContextFor: aCommand)
		prepareNewCommand: aCommand
]

{ #category : #testing }
CmdCompositeToolContext >> representsSimpleAnnotationUser: aToolClass [

	^existingContexts anySatisfy: [:each |
		each representsSimpleAnnotationUser: aToolClass]
]
