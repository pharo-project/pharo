Class {
	#name : 'SycCmDuplicateClassCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'activation' }
SycCmDuplicateClassCommand class >> browserShortcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation
		  by: $d meta
		  for: ClyClass asCalypsoItemContext
]

{ #category : 'testing' }
SycCmDuplicateClassCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isClassSelected
]

{ #category : 'execution' }
SycCmDuplicateClassCommand >> applyCommandResult [
	
	"Nothing"
]

{ #category : 'testing' }
SycCmDuplicateClassCommand >> canBeExecuted [

	^ context lastSelectedClass isTrait not
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

{ #category : 'testing' }
SycCmDuplicateClassCommand >> isComplexRefactoring [
	
	^ false
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
