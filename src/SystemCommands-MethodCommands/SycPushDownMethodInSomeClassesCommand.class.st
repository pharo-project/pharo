"
I am a command that pushes down methods in some classes.

I am responsible for delegating execution to `RBPushDownMethodInSomeClassesDriver`.
"
Class {
	#name : 'SycPushDownMethodInSomeClassesCommand',
	#superclass : 'SycRefactoringMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'converting' }
SycPushDownMethodInSomeClassesCommand >> asRefactorings [

	self shouldNotImplement
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> defaultMenuIconName [
	^ #down
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> defaultMenuItemName [
	^'Push down in some classes'
]

{ #category : 'execution' }
SycPushDownMethodInSomeClassesCommand >> executeRefactorings [

	(RePushDownMethodInSomeClassesDriver
		model: model
		scopes: refactoringScopes
		pushDownMethods: methods) runRefactoring
]

{ #category : 'testing' }
SycPushDownMethodInSomeClassesCommand >> isComplexRefactoring [ 

	^ false
]

{ #category : 'execution' }
SycPushDownMethodInSomeClassesCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes

]
