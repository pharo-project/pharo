"
I am a command to move methods to given class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : 'SycMoveMethodsToClassCommand',
	#superclass : 'SycMethodCmCommand',
	#instVars : [
		'targetClass'
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

{ #category : 'execution' }
SycMoveMethodsToClassCommand >> executeRefactoring [

	self methods
		collect: [ :method | (ReMoveMethodToClassDriver newApplication: self application) scopes: self refactoringScopes method: method class: targetClass ]
		thenDo: [ :refactoring | refactoring runRefactoring ].
	self methods do: [ :method | method origin removeEmptyProtocols ]
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> name [

	^ 'Move to another class'
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> order [

	^ 60
]

{ #category : 'executing' }
SycMoveMethodsToClassCommand >> prepareFullExecution [
	| toolContext |
	super prepareFullExecution.
	toolContext := self dropContext ifNil: [ self context ].
	targetClass := self dropContext
		ifNil: [ self context requestSingleClass: 'Choose class' ]
		ifNotNil: [ :drop | drop lastSelectedClass ].
	targetClass := toolContext currentMetaLevelOf: targetClass.
	self refactoringScopes: toolContext refactoringScopes
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycMoveMethodsToClassCommand >> targetClass: anObject [
	targetClass := anObject
]
