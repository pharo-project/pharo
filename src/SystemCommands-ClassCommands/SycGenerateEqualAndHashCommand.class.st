Class {
	#name : 'SycGenerateEqualAndHashCommand',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'variables'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycGenerateEqualAndHashCommand class >> canBeExecutedInContext: aToolContext [
	"Show the receiver if the last selected class has instance variables" 

	^ (super canBeExecutedInContext: aToolContext) and: [ 
		aToolContext lastSelectedClass instanceVariables notEmpty ]

]

{ #category : 'execution' }
SycGenerateEqualAndHashCommand >> asRefactorings [

	^{
		self refactoringClass
			className: targetClass name
			variables: (variables collect: [:each | each name])
	}
]

{ #category : 'accessing' }
SycGenerateEqualAndHashCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'execution' }
SycGenerateEqualAndHashCommand >> execute [
	self executeRefactorings
]

{ #category : 'execution' }
SycGenerateEqualAndHashCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	variables := aToolContext
		requestMultipleVariables: 'Choose variables for equality'
		from: { targetClass }.
	variables
		ifEmpty: [ 
			self inform: 'Please select variables for equality'.
			CmdCommandAborted signal ]
]

{ #category : 'factory method' }
SycGenerateEqualAndHashCommand >> refactoringClass [

	^ RBGenerateEqualHashTransformation
]
