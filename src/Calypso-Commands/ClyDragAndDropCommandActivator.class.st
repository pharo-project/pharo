"
I am the activator of a command which is dragged and dropped. I keep the drop context where the command is executed, and I run the drag preparation before the drop preparation, so the command has the state of both the drag and the drop.
"
Class {
	#name : 'ClyDragAndDropCommandActivator',
	#superclass : 'ClyCommandActivator',
	#instVars : [
		'dropContext'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'testing' }
ClyDragAndDropCommandActivator >> canExecuteCommand [

	^super canExecuteCommand and: [ self canExecuteDropInContext: dropContext ]
]

{ #category : 'testing' }
ClyDragAndDropCommandActivator >> canExecuteDropInContext: aToolContext [
	^ activationStrategy isDropActiveInContext: aToolContext
]

{ #category : 'executing' }
ClyDragAndDropCommandActivator >> executeCommand [
	[ command context: context.
		command dropContext: dropContext.
		command execute ]
	on: Exception do: [ :exc | context processFailure: exc of: command ]
]

{ #category : 'execution' }
ClyDragAndDropCommandActivator >> executeDropInContext: aToolContext [
	dropContext := aToolContext.
	self executeCommand
]
