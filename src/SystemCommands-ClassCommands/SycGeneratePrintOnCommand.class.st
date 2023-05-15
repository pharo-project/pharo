Class {
	#name : #SycGeneratePrintOnCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'variables'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #execution }
SycGeneratePrintOnCommand >> asRefactorings [

	^{
		self refactoringClass
			className: targetClass name
			variables: (variables collect: [:each | each name])
	}
]

{ #category : #accessing }
SycGeneratePrintOnCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."
	^ self refactoringClass menuItemString
]

{ #category : #execution }
SycGeneratePrintOnCommand >> execute [
	self executeRefactorings
]

{ #category : #execution }
SycGeneratePrintOnCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	variables := aToolContext
		requestMultipleVariables: 'Choose variables for print string'
		from: {targetClass}
]

{ #category : #'factory method' }
SycGeneratePrintOnCommand >> refactoringClass [

	^ RBGeneratePrintOnTransformation
]
