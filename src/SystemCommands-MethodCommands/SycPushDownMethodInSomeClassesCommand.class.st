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

{ #category : 'execution' }
SycPushDownMethodInSomeClassesCommand >> executeRefactorings [

	(RePushDownMethodInSomeClassesDriver
		model: model
		scopes: refactoringScopes
		pushDownMethods: methods) runRefactoring
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> iconName [
	^ #down
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> name [
	^'Push down in some classes'
]

{ #category : 'execution' }
SycPushDownMethodInSomeClassesCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes

]
