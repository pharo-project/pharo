Class {
	#name : 'SycSourceCodeRefactoringCommand',
	#superclass : 'SycSourceCodeCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycSourceCodeRefactoringCommand class >> isAbstract [

	^ self == SycSourceCodeRefactoringCommand
]

{ #category : 'converting' }
SycSourceCodeRefactoringCommand >> asRefactorings [
	self subclassResponsibility
]

{ #category : 'execution' }
SycSourceCodeRefactoringCommand >> execute [

	self executeRefactoring: self asRefactorings
]

{ #category : 'private' }
SycSourceCodeRefactoringCommand >> executeRefactoring: refactoring [

	[ refactoring first execute ]
		on: RBRefactoringError
		do: [ :e | self morphicUIManager alert: e messageText ]
]

{ #category : 'execution' }
SycSourceCodeRefactoringCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes.
]
