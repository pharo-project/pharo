"
I am a command to pull up given methods
"
Class {
	#name : 'SycPullUpMethodCommand',
	#superclass : 'SycRefactoringMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycPullUpMethodCommand class >> canBeExecutedInContext: aToolContext [

	^aToolContext isMethodSelected and: [ aToolContext lastSelectedClass superclass ~= Object ]
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> defaultMenuIconName [
	^ #up
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> defaultMenuItemName [
	^'(R) Pull up'
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> description [

	^ 'Pull up methods.'
]

{ #category : 'execution' }
SycPullUpMethodCommand >> executeRefactorings [

	(RePullUpMethodDriver
		 model: model
		 scopes: refactoringScopes
		 pullUpMethods: methods) runRefactoring
]

{ #category : 'testing' }
SycPullUpMethodCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycPullUpMethodCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes
]
