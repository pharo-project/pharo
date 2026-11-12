"
Pull up methods.
"
Class {
	#name : 'SycPullUpMethodCommand',
	#superclass : 'SycMethodRefactoringCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycPullUpMethodCommand >> canBeExecuted [
	^ super canBeExecuted and: [ self context lastSelectedClass superclass ~= Object ]
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
SycPullUpMethodCommand >> iconName [

	^ #up
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> name [
	^ '(R) Pull up'
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> order [
	^ 10
]
