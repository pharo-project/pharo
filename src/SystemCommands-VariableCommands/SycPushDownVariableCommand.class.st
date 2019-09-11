"
I am a command to push down given variables
"
Class {
	#name : #SycPushDownVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycPushDownVariableCommand >> asRefactorings [

	^self 
		createRefactorings: RBPushDownInstanceVariableRefactoring
]

{ #category : #accessing }
SycPushDownVariableCommand >> defaultMenuIconName [ 
	^ #down
]

{ #category : #accessing }
SycPushDownVariableCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : #execution }
SycPushDownVariableCommand >> prepareFullExecutionInContext: aToolContext [
	| noUsers |
	super prepareFullExecutionInContext: aToolContext.
	
	noUsers := aToolContext confirmUnusedVariablesInDefiningClass: variables.
	noUsers ifFalse: [ CmdCommandAborted signal ]
]
