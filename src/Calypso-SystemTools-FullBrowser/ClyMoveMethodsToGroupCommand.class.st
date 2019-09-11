"
I am a command to move methods to the method group.
By default I am activated by dragging selected method into the method tag (protocol).
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodGroup:		<ClyMethodGroup>
"
Class {
	#name : #ClyMoveMethodsToGroupCommand,
	#superclass : #SycMethodCommand,
	#instVars : [
		'methodGroup',
		'targetClass'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Methods'
}

{ #category : #activation }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethod asCalypsoItemContext 
		toDropIn: ClyTaggedMethodGroup asCalypsoItemContext
]

{ #category : #activation }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation2 [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethod asCalypsoItemContext 
		toDropIn: ClyAllMethodGroup asCalypsoItemContext
]

{ #category : #activation }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation3 [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethod asCalypsoItemContext 
		toDropIn: ClyExternalPackageMethodGroup asCalypsoItemContext
]

{ #category : #accessing }
ClyMoveMethodsToGroupCommand >> defaultMenuItemName [
	^'Move to method group'
]

{ #category : #execution }
ClyMoveMethodsToGroupCommand >> execute [
	
	| methodsOfAnotherClass |
	methodsOfAnotherClass := methods reject: [ :each | each origin = targetClass ].
	methodsOfAnotherClass ifNotEmpty: [ 
		(SycMoveMethodsToClassCommand methods: methodsOfAnotherClass class: targetClass) execute].
	
	methodGroup importMethods: methods
]

{ #category : #accessing }
ClyMoveMethodsToGroupCommand >> methodGroup [
	^ methodGroup
]

{ #category : #accessing }
ClyMoveMethodsToGroupCommand >> methodGroup: anObject [
	methodGroup := anObject
]

{ #category : #'drag and drop support' }
ClyMoveMethodsToGroupCommand >> prepareExecutionInDropContext: aToolContext [
	| selectedClasses |
	super prepareExecutionInDropContext: aToolContext.
	methodGroup := aToolContext lastSelectedMethodGroup.
	selectedClasses := aToolContext selectedClasses. 
	selectedClasses := selectedClasses collect: [:each | aToolContext currentMetaLevelOf: each].
	targetClass := selectedClasses size > 1 
		ifTrue: [ aToolContext 
			requestSingleClass: 'In what class you want to move method?' from: selectedClasses ]
		ifFalse: [ selectedClasses last ]
]

{ #category : #accessing }
ClyMoveMethodsToGroupCommand >> targetClass [
	^ targetClass
]

{ #category : #accessing }
ClyMoveMethodsToGroupCommand >> targetClass: anObject [
	targetClass := anObject
]
