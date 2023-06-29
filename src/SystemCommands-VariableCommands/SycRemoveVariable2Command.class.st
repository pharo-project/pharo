Class {
	#name : #SycRemoveVariable2Command,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #activation }
SycRemoveVariable2Command class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.5 for: ClySourceCodeContext
]

{ #category : #accessing }
SycRemoveVariable2Command >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
SycRemoveVariable2Command >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
SycRemoveVariable2Command >> execute [

	(RBRemoveInstanceVariableDriver 
		new
			scopes: toolContext refactoringScopes
			variable: variables first name 
			for: variables first definingClass 
		) execute
]

{ #category : #testing }
SycRemoveVariable2Command >> isComplexRefactoring [

	^false
]

{ #category : #execution }
SycRemoveVariable2Command >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.


]
