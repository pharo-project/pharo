Class {
	#name : 'SycCmDuplicateClassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmDuplicateClassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"
	| driver |

	driver := (ReDuplicateClassDriver className: self targetClass name) scopes: refactoringScopes.
	driver runRefactoring ifNil: [ ^ self ].
	(self confirm: 'Do you want to browse the new class?')
		ifTrue: [ context browser class openOnClass: (driver model classNamed: driver newClassName) ].

]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'testing' }
SycCmDuplicateClassCommand >> isApplicable [

	^ context lastSelectedClass isTrait not
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
