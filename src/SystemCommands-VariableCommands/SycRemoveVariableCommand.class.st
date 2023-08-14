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
