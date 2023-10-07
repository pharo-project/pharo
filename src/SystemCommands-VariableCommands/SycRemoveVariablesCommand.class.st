"
I am a command that is used for removing variables.

My responsibility is to delegate removal execution to the remove variable driver.
"
Class {
	#name : 'SycRemoveVariablesCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycRemoveVariablesCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.5 for: ClySourceCodeContext
]

{ #category : 'accessing' }
SycRemoveVariablesCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : 'accessing' }
SycRemoveVariablesCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : 'execution' }
SycRemoveVariablesCommand >> execute [

	variables first isClassVariable
		ifFalse: [
			(RBRemoveInstanceVariablesDriver new
				 scopes: toolContext refactoringScopes
				 variables: (variables collect: [:each | each name])
				 for: variables first definingClass) runRefactoring ]
		ifTrue: [
			"should be migrated to work on multiple elements"
			(RBRemoveSharedVariableDriver new
				 scopes: toolContext refactoringScopes
				 variable: variables first name
				 for: variables first definingClass) runRefactoring ]
]

{ #category : 'testing' }
SycRemoveVariablesCommand >> isComplexRefactoring [

	^false
]
