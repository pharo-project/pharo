Class {
	#name : 'SycCmDuplicateClassCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'execution' }
SycCmDuplicateClassCommand >> applyCommandResult [
	
	"Nothing"
]

{ #category : 'testing' }
SycCmDuplicateClassCommand >> canBeExecuted [

	^ self context isClassSelected and: [ self context lastSelectedClass isTrait not ]
]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> description [

	^ 'Duplicate Class.'
]

{ #category : 'executing' }
SycCmDuplicateClassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"

	| driver |
	driver := (ReDuplicateClassDriver className: self targetClass name) scopes: { (ClyClassScope of: self targetClass) }.
	driver runRefactoring
]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> name [

	^ 'Duplicate class'
]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> order [
	^ 11
]

{ #category : 'preparation' }
SycCmDuplicateClassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
