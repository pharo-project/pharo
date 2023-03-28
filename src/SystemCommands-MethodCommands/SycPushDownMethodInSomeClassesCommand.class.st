"
I am a command that pushes down methods in some classes.

I am responsible for delegating execution to `RBPushDownMethodInSomeClassesDriver`.
"
Class {
	#name : #SycPushDownMethodInSomeClassesCommand,
	#superclass : #SysRefactoringMethodCommand,
	#instVars : [
		'refactoringScopes'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #converting }
SycPushDownMethodInSomeClassesCommand >> asRefactorings [
	"Return push down method refactoring using method"
	| refactoring |
	refactoring := RBPushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: methods first origin
		in: self classes.
	^ OrderedCollection with: refactoring
]

{ #category : #converting }
SycPushDownMethodInSomeClassesCommand >> classes [
	^ classes ifNil: [ classes := #() ]
]

{ #category : #accessing }
SycPushDownMethodInSomeClassesCommand >> defaultMenuIconName [
	^ #down
]

{ #category : #accessing }
SycPushDownMethodInSomeClassesCommand >> defaultMenuItemName [
	^'Push down in some classes'
]

{ #category : #execution }
SycPushDownMethodInSomeClassesCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	self selectMethodsAndClasses
]
