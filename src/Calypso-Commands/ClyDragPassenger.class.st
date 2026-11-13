"
I am the object carried by a drag operation in a Calypso tool. I keep the context of the drag and the activators of the commands which can be dropped. I can tell whether a drop context accepts the dragged item, and I execute the drop on the first activator that accepts it.
"
Class {
	#name : 'ClyDragPassenger',
	#superclass : 'Object',
	#instVars : [
		'dragContext',
		'dropActivators'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyDragPassenger class >> draggedFromContext: aToolContext toActivate: commandActivators [

	^self new
		dragContext: aToolContext;
		dropActivators: commandActivators
]

{ #category : 'converting' }
ClyDragPassenger >> asDraggableMorph [
	^dragContext asDraggableMorph
]

{ #category : 'testing' }
ClyDragPassenger >> canBeDroppedInContext: aToolContext [

	^dropActivators anySatisfy: [ :each |
		each canExecuteDropInContext: aToolContext]
]

{ #category : 'accessing' }
ClyDragPassenger >> dragContext [
	^ dragContext
]

{ #category : 'accessing' }
ClyDragPassenger >> dragContext: anObject [
	dragContext := anObject
]

{ #category : 'accessing' }
ClyDragPassenger >> dropActivators [
	^ dropActivators
]

{ #category : 'accessing' }
ClyDragPassenger >> dropActivators: anObject [
	dropActivators := anObject
]

{ #category : 'dropping' }
ClyDragPassenger >> dropInContext: aToolContext [
	| targetActivator |
	targetActivator := dropActivators detect: [ :each |
		each canExecuteDropInContext: aToolContext].

	targetActivator executeDropInContext: aToolContext.

	^true
]
