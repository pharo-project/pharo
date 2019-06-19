"
I describe how access and execute command using drag and drop UI operation.

Add me to commands using:

	YourCommand>>yourApplicationDragAndDropActivation
		<classAnnotation>
		^CmdDragAndDropCommandActivation for: YourAppContextForDrag toDropIn: YourAppContextForDrop

First argument of activation strategy is a context where drap operation can be started. Last argument is a context where command can be executed by drop.

Applications which support me should implement few drag and drop methods according to the UI library (Morphic). At drag start a CmdDragPassenger should be created in current application context: 

	CmdDragAndDropCommandActivation createDragPassengerInContext:  aToolContext
	
Then at drop target widget (morph) the drop context should be created. It should be used to ask given passenger about possibility to execute the drop:
- aPassenger canBeDroppedInContext: targetToolContext 
- aPassanger dropInContext: targetToolContex 

To support all these methods passenger asks the command activator which delegate processing to underlying commands. Commands should define three methods:
- canBeExecutedInDropContext: aToolContext 
- prepareExecutionInDragContext: aToolContext
- prepareExecutionInDropContext: aToolContext

By default commands can be executed by any drop context and for preparation they do nothing. 

For command activation I return new kind of activator CmdDragAndDropCommandActivator which implements logic to execute commands in addition drop context of application. Look at it comment for details.

Internal Representation and Key Implementation Points.

    Instance Variables
	dropContextDefinition:		<CmdContextDefinition>
"
Class {
	#name : #CmdDragAndDropActivation,
	#superclass : #CmdCommandActivationStrategy,
	#instVars : [
		'activeDropContext'
	],
	#category : #'Commander-Activators-DragAndDrop'
}

{ #category : #'passenger creation' }
CmdDragAndDropActivation class >> createDragPassengerInContext: aToolContext [
	| activators |
	activators := self createActivatorsExecutableInContext: aToolContext.
		
	^CmdDragPassenger draggedFromContext: aToolContext toActivate: activators
]

{ #category : #'instance creation' }
CmdDragAndDropActivation class >> for: dragAnnotationContext toDropIn: dropAnnotationContext [
	^(self for: dragAnnotationContext)
		activeDropContext: dropAnnotationContext asAnnotationContext
]

{ #category : #accessing }
CmdDragAndDropActivation >> activeDropContext [
	^ activeDropContext
]

{ #category : #accessing }
CmdDragAndDropActivation >> activeDropContext: anObject [
	activeDropContext := anObject
]

{ #category : #activation }
CmdDragAndDropActivation >> defaultActivatorClass [
	^CmdDragAndDropCommandActivator
]

{ #category : #testing }
CmdDragAndDropActivation >> isDropActiveInContext: aToolContext [
	^ activeDropContext describes: aToolContext
]
