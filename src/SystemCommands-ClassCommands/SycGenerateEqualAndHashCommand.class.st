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
		self refactoringClass
			className: targetClass name
			variables: (variables collect: [:each | each name])
	}
]

{ #category : #accessing }
SycGenerateEqualAndHashCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
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

{ #category : #'factory method' }
SycGenerateEqualAndHashCommand >> refactoringClass [

	^ RBGenerateEqualHashTransformation
]
