"
I am a command to pull up given methods
"
Class {
	#name : 'SycPullUpMethodCommand',
	#superclass : 'SycMethodCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycPullUpMethodCommand >> canBeExecuted [
	^ super canBeExecuted and: [ self context lastSelectedClass superclass ~= Object ]
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> description [

	^ 'Pull up methods.'
]

{ #category : 'execution' }
SycPullUpMethodCommand >> executeRefactoring [

	^ ((RePullUpMethodDriver newApplication: self application)
		   model: model
		   scopes: self refactoringScopes
		   pullUpMethods: self methods)
		  runRefactoring
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> icon [
	^ self iconNamed: #up
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> name [
	^ '(R) Pull up'
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> order [
	^ 10
]
