"
I am a command to move methods to given class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : 'SycMoveMethodsToClassCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'targetClass',
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'instance creation' }
SycMoveMethodsToClassCommand class >> methods: methods class: targetClass [
	^(self for: methods)
		targetClass: targetClass
]

{ #category : 'instance creation' }
SycMoveMethodsToClassCommand class >> methods: methods class: targetClass refactoringScopes: refactoringScopes [
	"Create a command moving methods to a class without any tool context, which happens when another command delegates to me. Such a command has to be given the refactoring scopes explicitly, because I can not read them from a context."

	^ (self methods: methods class: targetClass) refactoringScopes: refactoringScopes
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> defaultMenuItemName [
	^'Move to another class'
]

{ #category : 'execution' }
SycMoveMethodsToClassCommand >> execute [

	methods
		collect: [ :each |(ReMoveMethodToClassDriver newApplication: self application) scopes: refactoringScopes method: each class: targetClass ]
		thenDo: [ :each | each runRefactoring ].
	methods do: [ :each | each origin removeEmptyProtocols ]
]

{ #category : 'drag and drop support' }
SycMoveMethodsToClassCommand >> prepareExecutionInDropContext: aToolContext [

	super prepareExecutionInDropContext: aToolContext.
	targetClass := aToolContext lastSelectedClass.
	targetClass := aToolContext currentMetaLevelOf: targetClass.
	refactoringScopes := aToolContext refactoringScopes
]

{ #category : 'execution' }
SycMoveMethodsToClassCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	targetClass := aToolContext requestSingleClass: 'Choose class'.
	targetClass := aToolContext currentMetaLevelOf: targetClass.
	refactoringScopes := aToolContext refactoringScopes
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> refactoringScopes: aCollection [

	refactoringScopes := aCollection
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> targetClass: anObject [
	targetClass := anObject
]
