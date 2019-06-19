Class {
	#name : #SycGenerateEqualAndHashCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'variables'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #execution }
SycGenerateEqualAndHashCommand >> asRefactorings [

	^{
		RBGenerateEqualHashRefactoring 
			className: targetClass name
			variables: (variables collect: [:each | each name])
	}
]

{ #category : #accessing }
SycGenerateEqualAndHashCommand >> defaultMenuItemName [
	^'Generate equal and hash'
]

{ #category : #execution }
SycGenerateEqualAndHashCommand >> execute [
	self executeRefactorings
]

{ #category : #execution }
SycGenerateEqualAndHashCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	variables := aToolContext 
		requestMultipleVariables: 'Choose variables for equality' 
		from: {targetClass}
]
