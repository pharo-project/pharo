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
	^ CmdShortcutActivation
		  by: $d meta
		  for: ClyClass asCalypsoItemContext
]

{ #category : 'testing' }
SycCmDuplicateClassCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isClassSelected
]

{ #category : 'execution' }
SycCmDuplicateClassCommand >> applyResultInContext: aClyFullBrowserClassContext [ 
	
	"Nothing"
]

{ #category : 'executing' }
SycCmDuplicateClassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"

	| driver |
	driver := (ReDuplicateClassDriver className: self targetClass name)
		          scopes: refactoringScopes.
	driver runRefactoring ifNil: [ ^ self ]
]

{ #category : 'accessing' }
SycCmDuplicateClassCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'testing' }
SycCmDuplicateClassCommand >> isApplicable [

	^ context lastSelectedClass isTrait not
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

{ #category : 'execution' }
SycCmDuplicateClassCommand >> prepareFullExecutionInContext: aClyFullBrowserClassContext [ 
	
	"Nothing"
]

{ #category : 'execution' }
SycCmDuplicateClassCommand >> readParametersFromContext: aToolContext [

	context := aToolContext
]
