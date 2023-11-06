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
	^#remove
]

{ #category : 'accessing' }
SycRemoveVariablesCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : 'execution' }
SycRemoveVariablesCommand >> execute [
	"Clearly from the UI we do not accept variables mixed from different classes and kinds"
	
	variables first isClassVariable
		ifFalse: [
			(RBRemoveInstanceVariablesDriver new
				 scopes: toolContext refactoringScopes
				 variables: (variables collect: [:each | each name])
				 for: variables first definingClass) runRefactoring ]
		ifTrue: [
			(RBRemoveSharedVariablesDriver new
				 scopes: toolContext refactoringScopes
				 variables: (variables collect: [:each | each name])
				 for: variables first definingClass) runRefactoring ]
]

{ #category : 'testing' }
SycRemoveVariablesCommand >> isComplexRefactoring [

	^false
]
