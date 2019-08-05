"
I am a command to generate accessors for given variables
"
Class {
	#name : #SycGenerateVariableAccessorCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycGenerateVariableAccessorCommand >> asRefactorings [

	^self
		createRefactorings: RBCreateAccessorsForVariableRefactoring 
		using: [ :refactoring :var |
			refactoring classVariable: var isClassVariable ]
]

{ #category : #accessing }
SycGenerateVariableAccessorCommand >> defaultMenuIconName [ 
	^ #smallAdd
]

{ #category : #accessing }
SycGenerateVariableAccessorCommand >> defaultMenuItemName [
	^'Generate accessors'
]
