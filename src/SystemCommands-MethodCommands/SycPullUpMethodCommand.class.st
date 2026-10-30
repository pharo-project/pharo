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
SycPullUpMethodCommand >> description [

	^ 'Pull up methods.'
]

{ #category : 'execution' }
SycPullUpMethodCommand >> executeRefactorings [

	^ ((RePullUpMethodDriver newApplication: self application) model: model scopes: refactoringScopes pullUpMethods: methods)
		  runRefactoring
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> iconName [
	^ #up
]

{ #category : 'accessing' }
SycPullUpMethodCommand >> name [
	^'(R) Pull up'
]

{ #category : 'execution' }
SycPullUpMethodCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes
]
