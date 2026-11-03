"
I am a command to protect given variables
"
Class {
	#name : 'SycProtectVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#instVars : [
		'selectedClass'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycProtectVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClySourceCodeContext
]

{ #category : 'execution' }
SycProtectVariableCommand >> execute [ 

	^ ((ReProtectInstanceVariableDriver newApplication: self application)
		   scopes: refactoringScopes 
		   variable: variables first
		   class: selectedClass) runRefactoring
]

{ #category : 'accessing' }
SycProtectVariableCommand >> iconName [
	^ #group
]

{ #category : 'accessing' }
SycProtectVariableCommand >> name [
	^'Protect'
]

{ #category : 'execution' }
SycProtectVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	selectedClass := aToolContext selectedVariables first definingClass.
]
