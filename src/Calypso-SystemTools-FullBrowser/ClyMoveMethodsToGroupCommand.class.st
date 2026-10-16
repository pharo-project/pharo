"
I am a command to move methods to the method group.
By default I am activated by dragging selected method into the method tag (protocol).
 
Methods that do not belong to the target class yet are moved to it with a move method refactoring, which needs the refactoring scopes of the browser. Since that refactoring is run by a command I build myself, I have to read the scopes in the drop context and hand them over.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodGroup:		<ClyMethodGroup>
	targetClass:		<Class>
	refactoringScopes:	<Collection of ClyNavigationScope>

"
Class {
	#name : 'ClyMoveMethodsToGroupCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'methodGroup',
		'targetClass',
		'refactoringScopes'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Methods',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Methods'
}

{ #category : 'activation' }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation [
	<classAnnotation>

	^CmdDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyMethodsInProtocolGroup asCalypsoItemContext
]

{ #category : 'activation' }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation2 [
	<classAnnotation>

	^CmdDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyAllMethodGroup asCalypsoItemContext
]

{ #category : 'activation' }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation3 [
	<classAnnotation>

	^CmdDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyExternalPackageMethodGroup asCalypsoItemContext
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> defaultMenuItemName [
	^'Move to method group'
]

{ #category : 'execution' }
ClyMoveMethodsToGroupCommand >> execute [

	| methodsOfAnotherClass |
	methodsOfAnotherClass := methods reject: [ :each | each origin = targetClass ].
	methodsOfAnotherClass ifNotEmpty: [
		(SycMoveMethodsToClassCommand methods: methodsOfAnotherClass class: targetClass refactoringScopes: refactoringScopes) execute ].
	methodGroup importMethods: methods
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> methodGroup [
	^ methodGroup
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> methodGroup: anObject [
	methodGroup := anObject
]

{ #category : 'drag and drop support' }
ClyMoveMethodsToGroupCommand >> prepareExecutionInDropContext: aToolContext [

	| selectedClasses |
	super prepareExecutionInDropContext: aToolContext.
	methodGroup := aToolContext lastSelectedMethodGroup.
	selectedClasses := aToolContext selectedClasses.
	selectedClasses := selectedClasses collect: [ :each | aToolContext currentMetaLevelOf: each ].
	targetClass := selectedClasses size > 1
		               ifTrue: [ aToolContext requestSingleClass: 'In what class you want to move method?' from: selectedClasses ]
		               ifFalse: [ selectedClasses last ].
	refactoringScopes := aToolContext refactoringScopes
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> targetClass: anObject [
	targetClass := anObject
]
