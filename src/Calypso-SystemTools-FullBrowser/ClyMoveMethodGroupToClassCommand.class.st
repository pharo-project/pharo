"
I am a command to move method group together with methods into another class.

By default I am activated by dragging method groups into the target class.
 
The methods of every group are moved with a move method refactoring, which needs the refactoring scopes of the browser. Since that refactoring is run by a command I build myself, I have to read the scopes in the drop context and hand them over. The refactoring already detaches the moved methods from their source and drops the protocol it leaves empty, so I must not remove the methods again myself.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
	refactoringScopes:	<Collection of ClyNavigationScope>


"
Class {
	#name : 'ClyMoveMethodGroupToClassCommand',
	#superclass : 'ClyMethodGroupCommand',
	#instVars : [
		'targetClass',
		'refactoringScopes'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'testing' }
ClyMoveMethodGroupToClassCommand class >> canBeExecutedInContext: aToolContext [

	^aToolContext isSelectedMethodGroupEditable
]

{ #category : 'activation' }
ClyMoveMethodGroupToClassCommand class >> fullBrowserDragAndDropActivation [
	<classAnnotation>

	^CmdDragAndDropActivation
		for: ClyMethodGroup asCalypsoItemContext
		toDropIn: ClyClass asCalypsoItemContext
]

{ #category : 'execution' }
ClyMoveMethodGroupToClassCommand >> execute [

	| moveCommand |
	methodGroups do: [ :each |
			moveCommand := SycMoveMethodsToClassCommand methods: each methods class: targetClass refactoringScopes: refactoringScopes.
			moveCommand execute ]
]

{ #category : 'execution' }
ClyMoveMethodGroupToClassCommand >> prepareExecutionInDropContext: aToolContext [

	super prepareExecutionInDropContext: aToolContext.
	targetClass := aToolContext lastSelectedClass.
	targetClass := aToolContext currentMetaLevelOf: targetClass.
	refactoringScopes := aToolContext refactoringScopes
]

{ #category : 'execution' }
ClyMoveMethodGroupToClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	targetClass := aToolContext requestSingleClass: 'Choose class'.
	targetClass := aToolContext currentMetaLevelOf: targetClass
]

{ #category : 'accessing' }
ClyMoveMethodGroupToClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
ClyMoveMethodGroupToClassCommand >> targetClass: anObject [
	targetClass := anObject
]
