"
I am a command to convert a temp variable in instance variable.
"
Class {
	#name : #SycConvertTempToInstVarCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #testing }
SycConvertTempToInstVarCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isTempVariableSelected  
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> defaultMenuIconName [ 
	^ #smallRedo
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> defaultMenuItemName [
	^'Convert to Instance Var'
]

{ #category : #execution }
SycConvertTempToInstVarCommand >> execute [
	| refactoring |

	refactoring := RBTemporaryToInstanceVariableRefactoring 
		class: method origin
		selector: method selector
		variable: sourceNode name.

	self executeRefactoring: refactoring.
]
