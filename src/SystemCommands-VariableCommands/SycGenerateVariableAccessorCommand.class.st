"
I am a command to generate accessors for given variables
"
Class {
	#name : 'SycGenerateVariableAccessorCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> defaultMenuIconName [
	^ #smallAdd
]

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'execution' }
SycGenerateVariableAccessorCommand >> execute [
	
	(ReGenerateAccessorsDriver new 
		targetClass: variables first definingClass;
		scopes: toolContext refactoringScopes;
		selectedVariables: (variables collect: [:each | each name ]) ) runRefactoring
	

]

{ #category : 'factory method' }
SycGenerateVariableAccessorCommand >> isComplexRefactoring [ 
	
	^ false
]

{ #category : 'execution' }
SycGenerateVariableAccessorCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]

{ #category : 'factory method' }
SycGenerateVariableAccessorCommand >> refactoringClass [

	^ ReCreateAccessorsForVariableTransformation
]
