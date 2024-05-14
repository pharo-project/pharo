Class {
	#name : 'SycPushUpMethodCommand2',
	#superclass : 'SycRefactoringMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycPushUpMethodCommand2 class >> canBeExecutedInContext: aToolContext [

	^aToolContext isMethodSelected and: [ aToolContext lastSelectedClass superclass ~= Object ]
]

{ #category : 'accessing' }
SycPushUpMethodCommand2 >> defaultMenuIconName [
	^ #up
]

{ #category : 'accessing' }
SycPushUpMethodCommand2 >> defaultMenuItemName [
	^'Push up 2'
]

{ #category : 'execution' }
SycPushUpMethodCommand2 >> executeRefactorings [

	(RePushUpMethodDriver
		 model: model
		 scopes: refactoringScopes
		 pushUpMethods: methods) runRefactoring
]

{ #category : 'testing' }
SycPushUpMethodCommand2 >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycPushUpMethodCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes
]
