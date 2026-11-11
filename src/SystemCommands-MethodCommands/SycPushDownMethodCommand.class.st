"
I am a command to push down given methods
"
Class {
	#name : 'SycPushDownMethodCommand',
	#superclass : 'SycMethodCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycPushDownMethodCommand >> asRefactorings [

	^ self shouldNotImplement
]

{ #category : 'accessing' }
SycPushDownMethodCommand >> description [

	^ 'Push down methods.'
]

{ #category : 'execution' }
SycPushDownMethodCommand >> executeRefactoring [

	^ ((RePushDownMethodDriver newApplication: self application) model: model scopes: self refactoringScopes pushDownMethods: self methods) runRefactoring
]

{ #category : 'accessing' }
SycPushDownMethodCommand >> icon [

	^ self iconNamed: #down
]

{ #category : 'accessing' }
SycPushDownMethodCommand >> name [

	^ 'Push down'
]

{ #category : 'accessing' }
SycPushDownMethodCommand >> order [

	^ 20
]
