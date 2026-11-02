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

{ #category : 'execution' }
SycProtectVariableCommand >> executeRefactoring [

	^ ((ReProtectInstanceVariableDriver newApplication: self application)
		scopes: refactoringScopes
		variable: variables first
		class: selectedClass) runRefactoring
]

{ #category : 'accessing' }
SycProtectVariableCommand >> fullBrowserOrder [

	^ 2000
]

{ #category : 'accessing' }
SycProtectVariableCommand >> icon [

	^ self iconNamed: #group
]

{ #category : 'accessing' }
SycProtectVariableCommand >> name [

	^ 'Protect'
]

{ #category : 'accessing' }
SycProtectVariableCommand >> order [

	^ 1.2
]

{ #category : 'execution' }
SycProtectVariableCommand >> prepareFullExecution [

	super prepareFullExecution.
	selectedClass := variables first definingClass
]
