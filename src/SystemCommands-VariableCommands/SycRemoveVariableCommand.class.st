Class {
	#name : #SycRemoveVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #activation }
SycRemoveVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.5 for: ClySourceCodeContext
]

{ #category : #accessing }
SycRemoveVariableCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
SycRemoveVariableCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
SycRemoveVariableCommand >> execute [

	variables first isClassVariable
		ifFalse: [
			(RBRemoveInstanceVariableDriver new
				 scopes: toolContext refactoringScopes
				 variable: variables first name
				 for: variables first definingClass) execute ]
		ifTrue: [
			(RBRemoveSharedVariableDriver new
				 scopes: toolContext refactoringScopes
				 variable: variables first name
				 for: variables first definingClass) execute ]
]

{ #category : #testing }
SycRemoveVariableCommand >> isComplexRefactoring [

	^false
]
