"
I represent passenger of drag and drop operation. I am created at start of drag operation  in current context of application with set of appropriate commands annotated by drag&drop activation strategies.
Then at drop target I detect most suitable command for given target context and execute it.
Look at CmdDragAndDropCommandActivation comment for details

Internal Representation and Key Implementation Points.

    Instance Variables
	dragContext:		<ToolContext>
	dropActivators:		<Collection of<CmdDragAndDropCommandActivator>>
"
Class {
	#name : #CmdDragPassenger,
	#superclass : #Object,
	#instVars : [
		'dragContext',
		'dropActivators'
	],
	#category : #'Commander-Activators-DragAndDrop'
}

{ #category : #'instance creation' }
CmdDragPassenger class >> draggedFromContext: aToolContext toActivate: commandActivators [

	^self new 
		dragContext: aToolContext;
		dropActivators: commandActivators 
]

{ #category : #converting }
CmdDragPassenger >> asDraggableMorph [
	^dragContext asDraggableMorph
]

{ #category : #testing }
CmdDragPassenger >> canBeDroppedInContext: aToolContext [

	^dropActivators anySatisfy: [ :each | 
		each canExecuteDropInContext: aToolContext]
]

{ #category : #accessing }
CmdDragPassenger >> dragContext [
	^ dragContext
]

{ #category : #accessing }
CmdDragPassenger >> dragContext: anObject [
	dragContext := anObject
]

{ #category : #accessing }
CmdDragPassenger >> dropActivators [
	^ dropActivators
]

{ #category : #accessing }
CmdDragPassenger >> dropActivators: anObject [
	dropActivators := anObject
]

{ #category : #dropping }
CmdDragPassenger >> dropInContext: aToolContext [
	| targetActivator |
	targetActivator := dropActivators detect: [ :each | 
		each canExecuteDropInContext: aToolContext].
	
	targetActivator executeDropInContext: aToolContext.
	
	^true
]
