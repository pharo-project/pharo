Class {
	#name : #SycAbstractVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#instVars : [
		'selectedClass'
	],
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycAbstractVariableCommand >> asRefactorings [

	^self
		createRefactorings: RBAbstractInstanceVariableRefactoring
		using: [ :refactoring :var |
			refactoring variable: var name class: selectedClass name ]
]

{ #category : #accessing }
SycAbstractVariableCommand >> defaultMenuIconName [
	^ #smallAdd
]

{ #category : #accessing }
SycAbstractVariableCommand >> defaultMenuItemName [

	^'Abstract accessors'
]

{ #category : #execution }
SycAbstractVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	selectedClass := aToolContext selectedClassSide.
	variables := aToolContext selectedVariables
]
