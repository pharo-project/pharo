"
I am a command to move method group together with methods into another class.

By default I am activated by dragging method groups into the target class.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>

"
Class {
	#name : #ClyMoveMethodGroupToClassCommand,
	#superclass : #ClyMethodGroupCommand,
	#instVars : [
		'targetClass'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #testing }
ClyMoveMethodGroupToClassCommand class >> canBeExecutedInContext: aToolContext [

	^aToolContext isSelectedMethodGroupEditable
]

{ #category : #activation }
ClyMoveMethodGroupToClassCommand class >> fullBrowserDragAndDropActivation [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethodGroup asCalypsoItemContext 
		toDropIn: ClyClass asCalypsoItemContext
]

{ #category : #execution }
ClyMoveMethodGroupToClassCommand >> execute [

	| moveCommand |
	methodGroups do: [ :each | 
		moveCommand := SycMoveMethodsToClassCommand methods: each methods class: targetClass.
		moveCommand execute.
		each removeWithMethods]
]

{ #category : #execution }
ClyMoveMethodGroupToClassCommand >> prepareExecutionInDropContext: aToolContext [
	super prepareExecutionInDropContext: aToolContext.
	targetClass := aToolContext lastSelectedClass.
	targetClass := aToolContext currentMetaLevelOf: targetClass
]

{ #category : #execution }
ClyMoveMethodGroupToClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	targetClass := aToolContext requestSingleClass: 'Choose class'.
	targetClass := aToolContext currentMetaLevelOf: targetClass
]

{ #category : #accessing }
ClyMoveMethodGroupToClassCommand >> targetClass [
	^ targetClass
]

{ #category : #accessing }
ClyMoveMethodGroupToClassCommand >> targetClass: anObject [
	targetClass := anObject
]
