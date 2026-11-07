"
I am a command that pushes down methods in some classes.

I am responsible for delegating execution to `RBPushDownMethodInSomeClassesDriver`.
"
Class {
	#name : 'SycPushDownMethodInSomeClassesCommand',
	#superclass : 'SycMethodCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'converting' }
SycPushDownMethodInSomeClassesCommand >> asRefactorings [

	self shouldNotImplement
]

{ #category : 'execution' }
SycPushDownMethodInSomeClassesCommand >> executeRefactoring [

	(RePushDownMethodInSomeClassesDriver model: model scopes: self refactoringScopes pushDownMethods: self methods) runRefactoring
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> icon [

	^ self iconNamed: #down
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> name [

	^ 'Push down in some classes'
]

{ #category : 'accessing' }
SycPushDownMethodInSomeClassesCommand >> order [

	^ 21
]
