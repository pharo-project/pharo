"
I am a command to convert a temp variable in instance variable.
"
Class {
	#name : #SycConvertTempToInstVarCommand,
	#superclass : #SycSourceCodeRefactoringCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #testing }
SycConvertTempToInstVarCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isTempVariableSelected  
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> asRefactorings [ 
	^ { RBTemporaryToInstanceVariableRefactoring 
		class: method origin
		selector: method selector
		variable: sourceNode name }
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> defaultMenuIconName [ 
	^ #smallRedo
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> defaultMenuItemName [
	^'Convert to Instance Var'
]
