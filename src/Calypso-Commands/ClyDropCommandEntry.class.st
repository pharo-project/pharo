"
I am a command declared for drag and drop in a Calypso command map. My anchor describes the dragged items and my drop anchor the contexts where they can be dropped:
	aCommandMap dragAndDrop add: SycMoveMethodsToClassCommand dropIn: ClyClass
My activator is a ClyDragAndDropCommandActivator.
"
Class {
	#name : 'ClyDropCommandEntry',
	#superclass : 'ClyCommandEntry',
	#instVars : [
		'dropAnchor'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyDropCommandEntry >> dropAnchor [
	"The anchor of the contexts where the dragged items can be dropped to run my command."

	^ dropAnchor
]

{ #category : 'accessing' }
ClyDropCommandEntry >> dropAnchor: anAnchorClass [

	dropAnchor := anAnchorClass
]

{ #category : 'activation' }
ClyDropCommandEntry >> isDropActiveInContext: aToolContext [

	^ aToolContext isDescribedByCommandAnchor: dropAnchor
]

{ #category : 'activation' }
ClyDropCommandEntry >> newActivatorFor: aToolContext [

	^ ClyDragAndDropCommandActivator new
		  command: (self newCommandInContext: aToolContext);
		  context: aToolContext;
		  activationStrategy: self;
		  yourself
]
