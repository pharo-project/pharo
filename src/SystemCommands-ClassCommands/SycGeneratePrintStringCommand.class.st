Class {
	#name : #SycGeneratePrintStringCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'variables'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #execution }
SycGeneratePrintStringCommand >> asRefactorings [

	^{
		RBGeneratePrintStringRefactoring 
			className: targetClass name
			variables: (variables collect: [:each | each name])
	}
]

{ #category : #accessing }
SycGeneratePrintStringCommand >> defaultMenuItemName [
	^'Generate print string'
]

{ #category : #execution }
SycGeneratePrintStringCommand >> execute [
	self executeRefactorings
]

{ #category : #execution }
SycGeneratePrintStringCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	variables := aToolContext 
		requestMultipleVariables: 'Choose variables for print string' 
		from: {targetClass}
]
