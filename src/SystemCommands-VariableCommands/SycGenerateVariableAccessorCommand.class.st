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
		createRefactorings: self refactoringClass
		using: [ :refactoring :var |
			refactoring classVariable: var isClassVariable ]
]

{ #category : #accessing }
SycGenerateVariableAccessorCommand >> defaultMenuIconName [
	^ #smallAdd
]

{ #category : #accessing }
SycGenerateVariableAccessorCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : #'factory method' }
SycGenerateVariableAccessorCommand >> refactoringClass [

	^ RBCreateAccessorsForVariableTransformation
]
