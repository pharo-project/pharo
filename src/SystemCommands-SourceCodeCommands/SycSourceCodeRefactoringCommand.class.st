Class {
	#name : 'SycSourceCodeRefactoringCommand',
	#superclass : 'SycSourceCodeCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
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

	self initializeDefaultOptionsOf: refactoring.
	[ refactoring execute ]
		on: RBRefactoringError
		do: [ :e | self morphicUIManager alert: e messageText ]
]

{ #category : 'testing' }
SycSourceCodeRefactoringCommand >> isComplexRefactoring [
	^ true
]
