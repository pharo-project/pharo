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

	^ClyDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyMethodsInProtocolGroup asCalypsoItemContext
]

{ #category : 'activation' }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation2 [
	<classAnnotation>

	^ClyDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyAllMethodGroup asCalypsoItemContext
]

{ #category : 'activation' }
ClyMoveMethodsToGroupCommand class >> fullBrowserDragAndDropActivation3 [
	<classAnnotation>

	^ClyDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: ClyExternalPackageMethodGroup asCalypsoItemContext
]

{ #category : 'execution' }
ClyMoveMethodsToGroupCommand >> executeRefactoring [

	| methodsOfAnotherClass |
	methodsOfAnotherClass := methods reject: [ :method | method origin = targetClass ].
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

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> name [
	^'Move to method group'
]

{ #category : 'executing' }
ClyMoveMethodsToGroupCommand >> prepareFullExecution [
	| selectedClasses |
	super prepareFullExecution.
	methodGroup := self dropContext lastSelectedMethodGroup.
	selectedClasses := self dropContext selectedClasses
		collect: [ :each | self dropContext currentMetaLevelOf: each ].
	targetClass := selectedClasses size > 1
		ifTrue: [ self dropContext requestSingleClass: 'In what class you want to move method?' from: selectedClasses ]
		ifFalse: [ selectedClasses last ].
	refactoringScopes := self dropContext refactoringScopes
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
ClyMoveMethodsToGroupCommand >> targetClass: anObject [
	targetClass := anObject
]
