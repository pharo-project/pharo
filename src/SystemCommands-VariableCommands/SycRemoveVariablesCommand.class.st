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
	"This is not really good since this is the job of the driver to invoke the refactoring 
	but we are transitionning"
	"The method menuItemString of the refactoring is extended to show if the action is a transformation or a refactoring
	Right now we do not invoke it because we call the driver and it means that we cannot honorate the menuItemString.
	Or it means that this user feedback should be expressed differently."
	
	^ 'Remove'
]

{ #category : 'accessing' }
SycRemoveVariablesCommand >> description [

	^ 'Removes Variables.'
]

{ #category : 'execution' }
SycRemoveVariablesCommand >> execute [
	"Clearly from the UI we do not accept variables mixed from different classes and kinds"
	
	variables first isClassVariable
		ifFalse: [
			(ReRemoveInstanceVariablesDriver new
				 scopes: toolContext refactoringScopes
				 variables: (variables collect: [:each | each name])
				 for: variables first definingClass) runRefactoring ]
		ifTrue: [
			(ReRemoveSharedVariablesDriver new
				 scopes: toolContext refactoringScopes
				 variables: (variables collect: [:each | each name])
				 for: variables first definingClass) runRefactoring ]
]

{ #category : 'testing' }
SycRemoveVariablesCommand >> isComplexRefactoring [

	^false
]
