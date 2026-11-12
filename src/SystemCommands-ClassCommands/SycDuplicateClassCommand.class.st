"
Duplicate Class.
"
Class {
	#name : 'SycDuplicateClassCommand',
	#superclass : 'SycClassRefactoringCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'execution' }
SycDuplicateClassCommand >> applyCommandResult [
	
	"Nothing"
]

{ #category : 'testing' }
SycDuplicateClassCommand >> canBeExecuted [

	^ self context isClassSelected and: [ self context lastSelectedClass isTrait not ]
]

{ #category : 'executing' }
SycDuplicateClassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"

	| driver |
	driver := (ReDuplicateClassDriver className: self targetClass name) scopes: { (ClyClassScope of: self targetClass) }.
	driver runRefactoring
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> iconName [

	^ #smallCopy
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> name [

	^ 'Duplicate class'
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> order [
	^ 11
]

{ #category : 'preparation' }
SycDuplicateClassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
