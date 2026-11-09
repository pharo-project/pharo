"
I am a command activation which runs a command by dragging an item of a tool and dropping it on another item. The drag side is the context of my class annotation, and the drop side is my active drop context. My activators are ClyDragAndDropCommandActivator.
"
Class {
	#name : 'ClyDragAndDropActivation',
	#superclass : 'ClyCommandActivationStrategy',
	#instVars : [
		'activeDropContext'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'passenger creation' }
ClyDragAndDropActivation class >> createDragPassengerInContext: aToolContext [
	| activators |
	activators := self createActivatorsExecutableInContext: aToolContext.

	^ClyDragPassenger draggedFromContext: aToolContext toActivate: activators
]

{ #category : 'instance creation' }
ClyDragAndDropActivation class >> for: dragAnnotationContext toDropIn: dropAnnotationContext [
	^(self for: dragAnnotationContext)
		activeDropContext: dropAnnotationContext asAnnotationContext
]

{ #category : 'accessing' }
ClyDragAndDropActivation >> activeDropContext [
	^ activeDropContext
]

{ #category : 'accessing' }
ClyDragAndDropActivation >> activeDropContext: anObject [
	activeDropContext := anObject
]

{ #category : 'activation' }
ClyDragAndDropActivation >> defaultActivatorClass [
	^ClyDragAndDropCommandActivator
]

{ #category : 'testing' }
ClyDragAndDropActivation >> isDropActiveInContext: aToolContext [
	^ activeDropContext describes: aToolContext
]
